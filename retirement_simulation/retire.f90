! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      retire.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-20
!! @brief     Retirement Simulator.@EOL
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
!! @filedetails   
!! 
!!     Documentation: https://richmit.github.io/FortranFinance/retirement_simulation/index.html
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!


!----------------------------------------------------------------------------------------------------------------------------------
program retire
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  use mrffl_percentages, only: p_of=>percentage_of, add_p=>add_percentage, percentage_of_total
  use mrffl_us_taxes, only: seed_tax_year, tax, std_tax_deduction_single, std_tax_deduction_joint, tax_bracket_breaks_single, tax_bracket_rates, tax_bracket_breaks_joint
  use mrffl_tvm, only: tvm_geometric_annuity_sum_a
  use mrffl_us_markets, only: snp_resample
  use mrffl_us_inflation, only: inf_resample

  implicit none

  ! Configuration File Parameters
  integer(kind=ik)  :: monte_carlo_years                   = 40         
  integer(kind=ik)  :: monte_carlo_runs                    = 10000      

  real(kind=rk)     :: initial_brokerage_balance           = 0.0  
  real(kind=rk)     :: initial_ira_balance_p2              = 0.0  
  real(kind=rk)     :: initial_ira_balance_p1              = 0.0  
  real(kind=rk)     :: initial_roth_balance_p2             = 0.0
  real(kind=rk)     :: initial_roth_balance_p1             = 0.0

  real(kind=rk)     :: high_investment_p                   = 100.0
  real(kind=rk)     :: mid_investment_p                    = 0.0
  real(kind=rk)     :: low_investment_p                    = 0.0
                                                      
  real(kind=rk)     :: high_investment_apr                 = 5.0
  real(kind=rk)     :: mid_investment_apr                  = 0.0
  real(kind=rk)     :: low_investment_apr                  = 0.0
  real(kind=rk)     :: cash_position_growth                = 2.5

  real(kind=rk)     :: initial_cash_reserves               = 0.0   
  real(kind=rk)     :: cash_reserves_growth                = 0.01       

  real(kind=rk)     :: initial_emergency_fund              = 0.0   
  real(kind=rk)     :: emergency_fund_growth               = 3.0        

  real(kind=rk)     :: first_year_tax                      = 0.0    

  real(kind=rk)     :: worst_case_inflation_rate           = 5.0        
  real(kind=rk)     :: fixed_inflation_rate                = 3.0        

  real(kind=rk)     :: initial_expected_annual_expenses    = 0.0    

  integer(kind=ik)  :: social_security_start_age_p2        = 67         
  integer(kind=ik)  :: social_security_start_age_p1        = 67         
  real(kind=rk)     :: initial_social_security_monthly     = 1000.0     
  real(kind=rk)     :: social_security_growth              = -1.0

  real(kind=rk)     :: initial_gross_work_salary_p2        = 0.0   
  real(kind=rk)     :: initial_gross_work_salary_p1        = 0.0   
  real(kind=rk)     :: work_salary_growth                  = 2.0        

  real(kind=rk)     :: initial_annual_ira_contrib_base     = 23000.0    
  real(kind=rk)     :: initial_annual_ira_contrib_catchup  = 7000.0     
  real(kind=rk)     :: annual_ira_contrib_growth           = 0.0        

  real(kind=rk)     :: initial_annual_roth_contrib_base    = 23000.0    
  real(kind=rk)     :: initial_annual_roth_contrib_catchup = 7000.0     
  real(kind=rk)     :: annual_roth_contrib_growth          = 0.0

  real(kind=rk)     :: surplus_reinvest                    = 0.0        

  integer(kind=ik)  :: retirement_year_p1                  = 2035
  integer(kind=ik)  :: retirement_year_p2                  = 2035       
  integer(kind=ik)  :: birthday_p1                         = 1980
  integer(kind=ik)  :: birthday_p2                         = 1980
  integer(kind=ik)  :: life_expectancy_p1                  = 110
  integer(kind=ik)  :: life_expectancy_p2                  = 110        
  integer(kind=ik)  :: simulation_year_start               = seed_tax_year       

  ! Global runtime variables used by the simulation
  integer(kind=ik)  :: age_p1, age_p2, simulation_year_end, year, tmp_j, num_runs
  real(kind=rk)     :: brokerage_balance, ira_balance_p2, ira_balance_p1, emergency_fund, roth_balance_p2, roth_balance_p1
  real(kind=rk)     :: cash_reserves, cash_income, expected_annual_expenses

  call read_config();

  if (high_investment_p + mid_investment_p + low_investment_p - 100 > zero_epsilon) then
     error stop "Investment mix must sum to 100%"
  end if

  if ((high_investment_apr < 0) .or. (mid_investment_apr < 0) .or. (low_investment_apr < 0) .or. (fixed_inflation_rate < 0)) then
     num_runs = monte_carlo_runs
  else
     num_runs = 1
  end if

  do tmp_j=1,num_runs
     call main_sim(tmp_j)
  end do

contains

  !------------------------------------------------------------------------------------------------------------------------------
  subroutine main_sim(sim)
    implicit none
    integer(kind=ik), intent(in) :: sim

     real(kind=rk)     :: cur_std_tax_deduction_single, cur_std_tax_deduction_joint, social_security_monthly
     real(kind=rk)     :: gross_work_salary_p2, gross_work_salary_p1, annual_ira_contrib_base, annual_ira_contrib_catchup
     real(kind=rk)     :: annual_roth_contrib_base, annual_roth_contrib_catchup, cur_annual_roth_contrib_growth
     real(kind=rk)     :: ira_savings_p1, ira_savings_p2, gross_work_income_p1, gross_work_income_p2
     real(kind=rk)     :: work_income_p1, work_income_p2, ss_income_p1, ss_income_p2, roth_savings_p1, roth_savings_p2
     real(kind=rk)     :: taxable_income, tax_rate, tax_owed, cur_inflation_rate
     real(kind=rk)     :: cur_emergency_fund_growth
     real(kind=rk)     :: cur_work_salary_growth, cur_social_security_growth, cur_annual_ira_contrib_growth
     real(kind=rk)     :: cur_investment_apr(3), cur_investment_mix(3)
     real(kind=rk)     :: cr_paied_cash, cr_paied_savings, cr_paied_ira, cr_paied_roth
     real(kind=rk)     :: annual_expenses_paied_cash, annual_expenses_paied_savings, annual_expenses_paied_roth, annual_expenses_paied_ira
     real(kind=rk)     :: tax_paied_cash, tax_paied_savings, tax_paied_ira, tax_paied_roth
     real(kind=rk)     :: start_cash_income
     real(kind=rk)     :: cur_tax_bracket_breaks_single(size(tax_bracket_breaks_single))
     real(kind=rk)     :: cur_tax_bracket_breaks_joint(size(tax_bracket_breaks_joint))

     !                                         s   y a1 a2    cr         inf       CpI/ST/SI/SR  ef B I12 R12  apr          ss1/2 wrk1/2 sav1 sav2 exp epI/T/I/R  taxbl      bkt       tax tpI/T/SI/SR
     character(len=*), parameter  :: fmt_n = "(i7, 3(1x, i4), 1x, f12.2, 1x, f5.1, 4(1x, f10.2), 6(1x, f16.2), 3(1x, f6.1), 4(1x, f9.2), 2(1x, f8.2), 5(1x, f11.2), 1x, f14.2, 1x, f6.2, 5(1x, f14.2))"
     character(len=*), parameter  :: fmt_h = "(a7, 3(1x, a4), 1x, a12,   1x, a5,   4(1x, a10),   6(1x, a16),   3(1x, a6),   4(1x, a9),   2(1x, a8),   5(1x, a11),   1x, a14,   1x, a6,   5(1x, a14) )"

     if (sim == 1) then
        print fmt_h, &
             "Sim", "Year", "Age1", "Age2", & 
             "SavingsC", "Inf", "CPaidI", "CPaidST", "CPaidSI", "CPaidSR", &
             "SavingsE", "SavingsB", "SavingsI1", "SavingsI2", "SavingsR1", "SavingsR2", "aprH", "arpM", "aprL", &
             "SS1","SS2","Wrk1","Wrk2", &
             "Sav1", "Sav2", &
             "Expenses", "EPaidI", "EPaidST", "EPaidSI", "EPaidSR", &
             "Taxable", "Bkt", "Taxes", "TPaidI", "TPaidST", "TPaidSI", "TPaidSR"
     end if

     ! -----------------------------------------------------------------------------------------------------------------------------
     ! Main simulation loop initialization & loop
     simulation_year_end           = max(birthday_p1+life_expectancy_p1, birthday_p2+life_expectancy_p2, simulation_year_start+10)-1
     brokerage_balance             = initial_brokerage_balance    
     ira_balance_p2                = initial_ira_balance_p2
     ira_balance_p1                = initial_ira_balance_p1
     roth_balance_p2               = initial_roth_balance_p2
     roth_balance_p1               = initial_roth_balance_p1
     expected_annual_expenses      = initial_expected_annual_expenses   
     cur_std_tax_deduction_single  = std_tax_deduction_single   
     cur_std_tax_deduction_joint   = std_tax_deduction_joint    
     social_security_monthly       = initial_social_security_monthly     
     gross_work_salary_p2          = initial_gross_work_salary_p2       
     gross_work_salary_p1          = initial_gross_work_salary_p1       
     annual_ira_contrib_base       = initial_annual_ira_contrib_base   
     annual_ira_contrib_catchup    = initial_annual_ira_contrib_catchup
     annual_roth_contrib_base      = initial_annual_roth_contrib_base   
     annual_roth_contrib_catchup   = initial_annual_roth_contrib_catchup
     tax_owed                      = first_year_tax
     emergency_fund                = initial_emergency_fund
     cash_reserves                 = initial_cash_reserves
     cur_tax_bracket_breaks_single = tax_bracket_breaks_single
     cur_tax_bracket_breaks_joint  = tax_bracket_breaks_joint
     taxable_income                = -1.0
     tax_rate                      = -1.0
     cur_investment_mix            = [ high_investment_p, mid_investment_p, low_investment_p ]

     do year=simulation_year_start, simulation_year_end

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Fix a value for monte carlo variables
        if ((worst_case_inflation_rate < 0) .or. &
             (cash_reserves + emergency_fund + brokerage_balance + ira_balance_p1 + ira_balance_p2 + roth_balance_p1 + roth_balance_p2 < &
             tvm_geometric_annuity_sum_a(1+simulation_year_end-year, worst_case_inflation_rate, expected_annual_expenses))) then
           cur_investment_apr(1)  = alt_if_neg(high_investment_apr, snp_resample(monte_carlo_years))
           cur_investment_apr(2)  = alt_if_neg(mid_investment_apr,  cur_investment_apr(1)/2)
           cur_investment_apr(3)  = alt_if_neg(low_investment_apr,  max(0.0_rk, cur_investment_apr(1)/4))
        else
           ! we have so much money at this point we don't need to aggressively invest
           cur_investment_apr  = cash_position_growth
        end if

        ! MJR TODO NOTE <2025-01-01T22:11:50-0600> main_sim: FIX NEXT TWO LINES!!!
        cur_emergency_fund_growth        = alt_if_neg(emergency_fund_growth,      cur_investment_apr(3))
        cur_inflation_rate               = alt_if_neg(fixed_inflation_rate,       inf_resample(monte_carlo_years))
        cur_work_salary_growth           = alt_if_neg(work_salary_growth,         max(0.0_rk, cur_inflation_rate/2))
        cur_social_security_growth       = alt_if_neg(social_security_growth,     max(0.0_rk, cur_inflation_rate))
        cur_annual_ira_contrib_growth    = alt_if_neg(annual_ira_contrib_growth,  max(0.0_rk, cur_inflation_rate))
        cur_annual_roth_contrib_growth   = alt_if_neg(annual_roth_contrib_growth, max(0.0_rk, cur_inflation_rate))

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Figure out ages
        age_p1 = year - birthday_p1
        age_p2 = year - birthday_p2

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Income from work
        gross_work_income_p1 = 0
        if (year < retirement_year_p1) then
           gross_work_income_p1 = gross_work_salary_p1
        end if

        gross_work_income_p2 = 0
        if (year < retirement_year_p2) then
           gross_work_income_p2 = gross_work_salary_p2
        end if

        ! Contribution to ira from work income
        ira_savings_p1 = 0
        if (year < retirement_year_p1) then
           ira_savings_p1 = ira_savings_p1 + annual_ira_contrib_base
           if (age_p1 >= 50) then
              ira_savings_p1 = ira_savings_p1 + annual_ira_contrib_catchup
           end if
        end if

        ira_savings_p2 = 0
        if (year < retirement_year_p2) then
           ira_savings_p2 = ira_savings_p2 + annual_ira_contrib_base
           if (age_p2 >= 50) then
              ira_savings_p2 = ira_savings_p2 + annual_ira_contrib_catchup
           end if
        end if

        ! Contribution to roth from work income
        roth_savings_p1 = 0
        if (year < retirement_year_p1) then
           roth_savings_p1 = roth_savings_p1 + annual_roth_contrib_base
           if (age_p1 >= 50) then
              roth_savings_p1 = roth_savings_p1 + annual_roth_contrib_catchup
           end if
        end if

        roth_savings_p2 = 0
        if (year < retirement_year_p2) then
           roth_savings_p2 = roth_savings_p2 + annual_roth_contrib_base
           if (age_p2 >= 50) then
              roth_savings_p2 = roth_savings_p2 + annual_roth_contrib_catchup
           end if
        end if

        ! Income from work (gross received minus non-taxable bit)
        work_income_p1 = gross_work_income_p1 - ira_savings_p1
        work_income_p2 = gross_work_income_p2 - ira_savings_p2

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Income from social security
        ss_income_p1 = 0
        if ((age_p1 >= social_security_start_age_p1) .and. (age_p1 < life_expectancy_p1)) then
           ss_income_p1 = social_security_monthly * 12
        end if
        ss_income_p2 = 0
        if ((age_p2 >= social_security_start_age_p2) .and. (age_p2 < life_expectancy_p2)) then
           ss_income_p2 = social_security_monthly * 12
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Total income from non-savings sources
        cash_income = ss_income_p1 + ss_income_p2 + work_income_p1 + work_income_p2
        start_cash_income = cash_income

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Initialize taxable income.  It will be updated later for retirement account withdrawals
        taxable_income = cash_income                                                                                 ! Taxable Non-savings income
        taxable_income = taxable_income + sum(p_of(p_of(brokerage_balance, cur_investment_mix), cur_investment_apr)) ! Taxable investment income
        taxable_income = taxable_income + p_of(emergency_fund,    emergency_fund_growth)                             ! Taxable investment income
        taxable_income = taxable_income + p_of(cash_reserves, cash_reserves_growth)                                  ! Taxable investment income

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Pay annual expenses
        call pay_stuff(expected_annual_expenses, annual_expenses_paied_cash, annual_expenses_paied_savings, &
                       annual_expenses_paied_ira, annual_expenses_paied_roth, .true.)

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Pay Tax For Last Year
        call pay_stuff(tax_owed, tax_paied_cash, tax_paied_savings, tax_paied_ira, tax_paied_roth, .true.)

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Bump Cash Reserves
        call pay_stuff(p_of(cash_reserves, cur_inflation_rate), cr_paied_cash, cr_paied_savings, &
                       cr_paied_ira, cr_paied_roth, .false.)
        cash_reserves = cash_reserves + cr_paied_cash + cr_paied_savings + cr_paied_ira + cr_paied_roth

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Taxes From Payments
        taxable_income = taxable_income + tax_paied_ira
        taxable_income = taxable_income + annual_expenses_paied_ira
        taxable_income = taxable_income + cr_paied_ira

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Process investment income & contributions
        brokerage_balance = brokerage_balance + sum(p_of(p_of(brokerage_balance, cur_investment_mix), cur_investment_apr)) ! Taxable 
        roth_balance_p1   = roth_balance_p1   + sum(p_of(p_of(roth_balance_p1,   cur_investment_mix), cur_investment_apr)) ! Tax deferred
        roth_balance_p2   = roth_balance_p2   + sum(p_of(p_of(roth_balance_p2,   cur_investment_mix), cur_investment_apr)) ! Tax deferred
        ira_balance_p1    = ira_balance_p1    + sum(p_of(p_of(ira_balance_p1,    cur_investment_mix), cur_investment_apr)) ! Tax deferred
        ira_balance_p2    = ira_balance_p2    + sum(p_of(p_of(ira_balance_p2,    cur_investment_mix), cur_investment_apr)) ! Tax deferred
        emergency_fund    = emergency_fund    + p_of(emergency_fund, emergency_fund_growth)                                ! taxable saftey
        brokerage_balance = brokerage_balance + p_of(cash_income, surplus_reinvest)                                        ! Left over income
        cash_reserves     = cash_reserves     + p_of(cash_reserves, cash_reserves_growth)                                  ! taxable checking
        ira_balance_p1    = ira_balance_p1    + ira_savings_p1                                                             ! 401K contribution p1
        ira_balance_p2    = ira_balance_p2    + ira_savings_p2                                                             ! 401K contribution p2

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Figure out taxable income for *this year* to be paid *next year*
        if ((age_p1 < life_expectancy_p1) .and. (age_p2 < life_expectancy_p2)) then
           taxable_income = taxable_income - cur_std_tax_deduction_joint
        else
           taxable_income = taxable_income - cur_std_tax_deduction_single
        end if
        if (taxable_income < 0) then
           taxable_income = 0
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Print status to STDOUT
        print fmt_n, &
             sim, year, age_p1, age_p2, &
             cash_reserves, cur_inflation_rate, cr_paied_cash, cr_paied_savings, cr_paied_ira, cr_paied_roth, &
             emergency_fund, brokerage_balance, ira_balance_p1, ira_balance_p2,roth_balance_p1, roth_balance_p2, &
             cur_investment_apr(1), cur_investment_apr(2), cur_investment_apr(3), &
             ss_income_p1, ss_income_p2, gross_work_income_p1, gross_work_income_p2, &
             ira_savings_p1, ira_savings_p2, &
             expected_annual_expenses, annual_expenses_paied_cash, annual_expenses_paied_savings, &
             annual_expenses_paied_ira, annual_expenses_paied_roth, &
             taxable_income, tax_rate, tax_owed, tax_paied_cash, tax_paied_savings, tax_paied_ira, tax_paied_roth

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Compute Taxes For Next Year        
        if ((age_p1 < life_expectancy_p1) .and. (age_p2 < life_expectancy_p2)) then
           tax_owed = tax(taxable_income, cur_tax_bracket_breaks_joint, tax_bracket_rates)
        else
           tax_owed = tax(taxable_income, cur_tax_bracket_breaks_single, tax_bracket_rates)
        end if
        tax_rate = percentage_of_total(taxable_income, tax_owed)
        
        ! ------------------------------------------------------------------------------------------------------------------------
        ! Grow values for inflation, wage growth, ss growth, etc...
        expected_annual_expenses      = add_p(expected_annual_expenses,      cur_inflation_rate)             ! Inflation
        gross_work_salary_p2          = add_p(gross_work_salary_p2,          cur_work_salary_growth)         ! Raise at work
        gross_work_salary_p1          = add_p(gross_work_salary_p1,          cur_work_salary_growth)         ! Raise at work 
        social_security_monthly       = add_p(social_security_monthly,       cur_social_security_growth)     ! Raise for SS
        annual_ira_contrib_base       = add_p(annual_ira_contrib_base,       cur_annual_ira_contrib_growth)  ! Raise 401k contribution
        annual_ira_contrib_catchup    = add_p(annual_ira_contrib_catchup,    cur_annual_ira_contrib_growth)  ! Raise 401k contribution
        annual_roth_contrib_base      = add_p(annual_roth_contrib_base,      cur_annual_roth_contrib_growth) ! Raise 401k contribution
        annual_roth_contrib_catchup   = add_p(annual_roth_contrib_catchup,   cur_annual_roth_contrib_growth) ! Raise 401k contribution
        cur_std_tax_deduction_single  = add_p(cur_std_tax_deduction_single,  cur_inflation_rate)             ! Raise single tax deduction
        cur_std_tax_deduction_joint   = add_p(cur_std_tax_deduction_joint,   cur_inflation_rate)             ! Raise joint tax deduction
        cur_tax_bracket_breaks_single = add_p(cur_tax_bracket_breaks_single, cur_inflation_rate)             ! Raise tax bracket endpoints
        cur_tax_bracket_breaks_joint  = add_p(cur_tax_bracket_breaks_joint,  cur_inflation_rate)             ! Raise tax bracket endpoints

     end do
   end subroutine  main_sim

   !------------------------------------------------------------------------------------------------------------------------------
   real(kind=rk) function alt_if_neg(val1, val2)
     real(kind=rk), intent(in) :: val1
     real(kind=rk), intent(in) :: val2
     if (val1 < 0) then
        alt_if_neg = val2
     else
        alt_if_neg = val1
     end if
   end function alt_if_neg

   !------------------------------------------------------------------------------------------------------------------------------
   subroutine pay_stuff(bill, paid_from_cash, paid_from_savings, paid_from_ira, paid_from_roth, cash_reserves_eligible)
     implicit none
     real(kind=rk),    intent(in)  :: bill
     real(kind=rk),    intent(out) :: paid_from_cash, paid_from_savings, paid_from_ira, paid_from_roth
     logical,          intent(in)  :: cash_reserves_eligible
     real(kind=rk)                 :: paid
     real(kind=rk)                 :: ded
     ! Globals: cash_income, brokerage_balance, ira_balance_p1, ira_balance_p2, roth_balance_p1, roth_balance_p2
     ! Globals: age_p1, age_p2, emergency_fund, cash_reserves
     paid_from_cash     = 0
     paid_from_savings  = 0
     paid_from_ira      = 0
     paid_from_roth     = 0
     paid               = 0
     if ((paid < bill) .and. (cash_income > 0)) then                                ! Use SS & work income
        ded = min(bill, cash_income)
        paid = paid + ded
        paid_from_cash = paid_from_cash + ded
        cash_income = cash_income - ded
     end if
     if ((paid < bill) .and. (brokerage_balance > 0)) then                          ! Use brokerage_balance
        ded = min(bill - paid, brokerage_balance)
        paid = paid + ded
        paid_from_savings = paid_from_savings + ded
        brokerage_balance = brokerage_balance - ded
     end if
     if ((paid < bill) .and. (ira_balance_p1 > 0) .and. (60 <= age_p1)) then        ! Use ira_balance_p1
        ded = min(bill - paid, ira_balance_p1)
        paid = paid + ded
        paid_from_ira = paid_from_ira + ded
        ira_balance_p1 = ira_balance_p1 - ded
     end if
     if ((paid < bill) .and. (ira_balance_p2 > 0) .and. (60 <= age_p2)) then        ! Use ira_balance_p2
        ded = min(bill - paid, ira_balance_p2)
        paid = paid + ded
        paid_from_ira = paid_from_ira + ded
        ira_balance_p2 = ira_balance_p2 - ded
     end if
     if ((paid < bill) .and. (roth_balance_p1 > 0) .and. (60 <= age_p1)) then       ! Use roth_balance_p1
        ded = min(bill - paid, roth_balance_p1)
        paid = paid + ded
        paid_from_roth = paid_from_roth + ded
        roth_balance_p1 = roth_balance_p1 - ded
     end if
     if ((paid < bill) .and. (roth_balance_p2 > 0) .and. (60 <= age_p2)) then       ! Use roth_balance_p2
        ded = min(bill - paid, roth_balance_p2)
        paid = paid + ded
        paid_from_roth = paid_from_roth + ded
        roth_balance_p2 = roth_balance_p2 - ded
     end if
     if ((paid < bill) .and. (emergency_fund > 0)) then                             ! Use emergency_fund
        ded = min(bill - paid, emergency_fund)
        paid = paid + ded
        paid_from_savings = paid_from_savings + ded
        emergency_fund = emergency_fund - ded
     end if
     if (cash_reserves_eligible .and. (paid < bill) .and. (cash_reserves > 0)) then ! Use cash_reserves
        ded = min(bill - paid, cash_reserves)
        paid = paid + ded
        paid_from_savings = paid_from_savings + ded
        cash_reserves = cash_reserves - ded
     end if
   end subroutine pay_stuff

   !------------------------------------------------------------------------------------------------------------------------------   
   subroutine read_config()
     implicit none
     
     namelist /SIMPARM/ monte_carlo_years, monte_carlo_runs
     namelist /SIMPARM/ initial_annual_roth_contrib_base, initial_annual_roth_contrib_catchup,annual_roth_contrib_growth, cash_position_growth
     namelist /SIMPARM/ initial_brokerage_balance, initial_roth_balance_p1, initial_roth_balance_p2, initial_ira_balance_p1, initial_ira_balance_p2
     namelist /SIMPARM/ high_investment_p, mid_investment_p, low_investment_p
     namelist /SIMPARM/ high_investment_apr, mid_investment_apr, low_investment_apr
     namelist /SIMPARM/ initial_cash_reserves, cash_reserves_growth
     namelist /SIMPARM/ initial_emergency_fund, emergency_fund_growth
     namelist /SIMPARM/ first_year_tax
     namelist /SIMPARM/ worst_case_inflation_rate, fixed_inflation_rate
     namelist /SIMPARM/ initial_expected_annual_expenses
     namelist /SIMPARM/ social_security_start_age_p1, social_security_start_age_p2, initial_social_security_monthly, social_security_growth
     namelist /SIMPARM/ initial_gross_work_salary_p1, initial_gross_work_salary_p2, work_salary_growth
     namelist /SIMPARM/ initial_annual_ira_contrib_base, initial_annual_ira_contrib_catchup, annual_ira_contrib_growth
     namelist /SIMPARM/ surplus_reinvest
     namelist /SIMPARM/ retirement_year_p1, retirement_year_p2
     namelist /SIMPARM/ birthday_p1, birthday_p2
     namelist /SIMPARM/ life_expectancy_p1, life_expectancy_p2
 
     ! Variables for config file
     integer                       :: io_stat, io_unit, file_name_len
     character(len=512)            :: io_msg
     character(len=:), allocatable :: file_name

     call get_command(length=file_name_len)
     allocate(character(len=file_name_len) :: file_name)
     call get_command_argument(1, file_name, file_name_len)
     file_name = adjustl(trim(file_name))

     if (len(file_name) <= 0) then
        file_name = "retire.config"
     end if

     open(newunit=io_unit, file=file_name, form='formatted', action='read', iostat=io_stat, iomsg=io_msg)
     if (io_stat /= 0) then
        print "(a)", trim(io_msg)
        error stop "Unable to open config file"
     end if
     read (nml=SIMPARM, unit=io_unit, iostat=io_stat, iomsg=io_msg)
     if (io_stat > 0) then
        print "(a)", trim(io_msg)
        error stop "I/O Error reading configuration"
     end if
     close(unit=io_unit, status='keep', iostat=io_stat, iomsg=io_msg)
     if (io_stat /= 0) then
        print "(a)", trim(io_msg)
        error stop "I/O error closing file"
     end if
   end subroutine read_config

 end program retire

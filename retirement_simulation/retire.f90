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
  use, intrinsic :: iso_fortran_env,    only: output_unit, error_unit
  use            :: mrffl_config,       only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  use            :: mrffl_percentages,  only: p_of=>percentage_of, add_p=>add_percentage, percentage_of_total
  use            :: mrffl_us_taxes,     only: seed_tax_year, tax, tax_bracket_rates, std_tax_deduction_single, tax_bracket_breaks_single, std_tax_deduction_joint, tax_bracket_breaks_joint
  use            :: mrffl_tvm,          only: tvm_geometric_annuity_sum_a
  use            :: mrffl_stats,        only: rand_int
  use            :: mrffl_us_markets,   only: snp_resample, dgs10_resample, snp_dat, dgs10_dat
  use            :: mrffl_us_inflation, only: inf_resample, inf_dat
  use            :: mrffl_life_table,   only: life_expectancy, usss_f_lx_dat, usss_m_lx_dat, rand_age
  implicit none

  ! Configuration File Parameters
  integer(kind=ik)  :: monte_carlo_years                   = 40
  integer(kind=ik)  :: monte_carlo_runs                    = 10000

  real(kind=rk)     :: initial_brokerage_balance           = 0.0
  real(kind=rk)     :: initial_ira_balance_p1              = 0.0
  real(kind=rk)     :: initial_ira_balance_p2              = 0.0
  real(kind=rk)     :: initial_roth_balance_p1             = 0.0
  real(kind=rk)     :: initial_roth_balance_p2             = 0.0

  real(kind=rk)     :: high_investment_p                   = 100.0
  real(kind=rk)     :: mid_investment_p                    = 0.0
  real(kind=rk)     :: low_investment_p                    = 0.0

  real(kind=rk)     :: high_investment_apr                 = 5.0
  real(kind=rk)     :: mid_investment_apr                  = 4.0
  real(kind=rk)     :: low_investment_apr                  = 3.0
  real(kind=rk)     :: cash_position_growth                = 2.5

  real(kind=rk)     :: initial_cash_reserves               = 0.0
  real(kind=rk)     :: cash_reserves_growth                = 0.01

  real(kind=rk)     :: initial_emergency_fund              = 0.0
  real(kind=rk)     :: emergency_fund_growth               = 3.0

  real(kind=rk)     :: first_year_tax                      = 0.0

  real(kind=rk)     :: worst_case_inflation_rate           = 5.0
  real(kind=rk)     :: fixed_inflation_rate                = 3.0

  real(kind=rk)     :: initial_expected_expenses_shr       = 0.0
  real(kind=rk)     :: initial_expected_expenses_p1        = 0.0
  real(kind=rk)     :: initial_expected_expenses_p2        = 0.0

  integer(kind=ik)  :: social_security_start_age_p1        = 67
  integer(kind=ik)  :: social_security_start_age_p2        = 67
  real(kind=rk)     :: initial_social_security_monthly_p1  = 1000.0
  real(kind=rk)     :: initial_social_security_monthly_p2  = 1000.0
  real(kind=rk)     :: social_security_growth              = -1.0

  real(kind=rk)     :: initial_gross_work_salary_p1        = 0.0
  real(kind=rk)     :: initial_gross_work_salary_p2        = 0.0
  real(kind=rk)     :: work_salary_growth                  = 2.0

  real(kind=rk)     :: initial_annual_ira_contrib_base     = 23000.0
  real(kind=rk)     :: initial_annual_ira_contrib_catchup  = 7000.0
  real(kind=rk)     :: annual_ira_contrib_growth           = 0.0

  real(kind=rk)     :: initial_annual_roth_contrib_base    = 23000.0
  real(kind=rk)     :: initial_annual_roth_contrib_catchup = 7000.0
  real(kind=rk)     :: annual_roth_contrib_growth          = 0.0

  real(kind=rk)     :: target_taxable_income               = 0
  real(kind=rk)     :: minimum_roth_conversion             = 15000
  integer(kind=ik)  :: maximum_roth_conversion_year        = 0

  real(kind=rk)     :: surplus_reinvest                    = 0.0

  integer(kind=ik)  :: retirement_year_p1                  = 2035
  integer(kind=ik)  :: retirement_year_p2                  = 2035
  integer(kind=ik)  :: birthday_p1                         = 1980
  integer(kind=ik)  :: birthday_p2                         = 1980
  integer(kind=ik)  :: life_expectancy_p1                  = 110
  integer(kind=ik)  :: life_expectancy_p2                  = 110
  character(len=1)  :: sex_p1                              = 'F'
  character(len=1)  :: sex_p2                              = 'F'

  integer(kind=ik)  :: simulation_year_start               = seed_tax_year + 1

  integer(kind=ik)  :: verbosity                           = 10

  ! Global runtime variables used by the simulation
  integer(kind=ik)   :: age_p1, age_p2, simulation_year_end, year, tmp_j, num_runs, mc_year_low, mc_year_high
  integer(kind=ik)   :: last_roth_conversion_year_p1, last_roth_conversion_year_p2
  real(kind=rk)      :: brokerage_balance, ira_balance_p2, ira_balance_p1, emergency_fund, roth_balance_p2, roth_balance_p1
  real(kind=rk)      :: cash_reserves, cash_income
  real(kind=rk)      :: expected_expenses_shr, expected_expenses_p1, expected_expenses_p2, total_expected_expenses

  character(len=10)  :: out_file_name = 'retire.out'
  integer            :: out_io_stat, out_io_unit
  character(len=512) :: out_io_msg
  real               :: cpu_time_start, cpu_time_end

  call cpu_time(cpu_time_start)

  if (verbosity >= 10)write (output_unit, '(a40)') "Reading Config"
  call read_config();

  if ((sex_p1 /= 'F') .and. (sex_p1 /= 'M')) then
     error stop "Sex for p1 must be 'M' or 'F'."
  end if
  if ((sex_p2 /= 'F') .and. (sex_p2 /= 'M')) then
     error stop "Sex for p2 must be 'M' or 'F'."
  end if
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "simulation_year_start:", simulation_year_start
  if (life_expectancy_p1 == 0) then
     if (sex_p1 == 'F') then
        life_expectancy_p1 = ceiling(life_expectancy(simulation_year_start-birthday_p1, usss_f_lx_dat, 0_ik), kind=ik) + simulation_year_start - birthday_p1
     else
        life_expectancy_p1 = ceiling(life_expectancy(simulation_year_start-birthday_p1, usss_m_lx_dat, 0_ik), kind=ik) + simulation_year_start - birthday_p1
     end if
  end if
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "birthday_p1:", birthday_p1
  if (verbosity >= 30) write (output_unit, '(a40,a20)') "sex_p1:", sex_p1
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "life_expectancy_p1:", life_expectancy_p1
  if (life_expectancy_p2 == 0) then
     if (sex_p2 == 'F') then
        life_expectancy_p2 = ceiling(life_expectancy(simulation_year_start-birthday_p2, usss_f_lx_dat, 0_ik), kind=ik) + simulation_year_start - birthday_p2
     else
        life_expectancy_p2 = ceiling(life_expectancy(simulation_year_start-birthday_p2, usss_m_lx_dat, 0_ik), kind=ik) + simulation_year_start - birthday_p2
     end if
  end if
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "birthday_p2:", birthday_p2
  if (verbosity >= 30) write (output_unit, '(a40,a20)') "sex_p2:", sex_p2
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "life_expectancy_p2:", life_expectancy_p2


  if (high_investment_p + mid_investment_p + low_investment_p - 100 > zero_epsilon) then
     error stop "Investment mix must sum to 100%"
  end if

  if ((high_investment_apr < 0) .or. (mid_investment_apr < 0) .or. (low_investment_apr < 0) .or. (fixed_inflation_rate < 0) .or. (life_expectancy_p1 < 0) .or. (life_expectancy_p2 < 0)) then
     num_runs = monte_carlo_runs
  else
     num_runs = 1
  end if

  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC life_expectancy_p1:",   (life_expectancy_p1   < 0)
  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC life_expectancy_p2:",   (life_expectancy_p2   < 0)
  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC high_investment_apr:",  (high_investment_apr  < 0)
  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC mid_investment_apr:",   (mid_investment_apr   < 0)
  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC low_investment_apr:",   (low_investment_apr   < 0)
  if (verbosity >= 30) write (output_unit, '(a40,l20)') "MC fixed_inflation_rate:", (fixed_inflation_rate < 0)
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "num_runs:", num_runs

  mc_year_high = simulation_year_start + 10000_ik
  mc_year_high = min(mc_year_high, ubound(snp_dat, 1, kind=ik))
  mc_year_high = min(mc_year_high, ubound(dgs10_dat, 1, kind=ik))
  mc_year_high = min(mc_year_high, ubound(inf_dat, 1, kind=ik))
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "mc_year_high:", mc_year_high

  mc_year_low  = mc_year_high - monte_carlo_years
  mc_year_low  = max(mc_year_low,  lbound(snp_dat, 1, kind=ik))
  mc_year_low  = max(mc_year_low,  lbound(dgs10_dat, 1, kind=ik))
  mc_year_low  = max(mc_year_low,  lbound(inf_dat, 1, kind=ik))
  if (verbosity >= 30) write (output_unit, '(a40,i20)') "mc_year_low:", mc_year_low

  if ( (mc_year_high - mc_year_low) < monte_carlo_years) then
     error stop "Not enough historical data to support monte_carlo_years setting"
  end if

  if (verbosity >= 30) write (output_unit, '(a40,f20.2)') "Initial Assets:", (initial_brokerage_balance + initial_ira_balance_p1 + initial_ira_balance_p2 + initial_roth_balance_p1 + initial_roth_balance_p2 + initial_cash_reserves + initial_emergency_fund)

  open(newunit=out_io_unit, file=out_file_name, form='formatted', action='write', iostat=out_io_stat, iomsg=out_io_msg)
  if (out_io_stat /= 0) then
     write (error_unit, '(a)') trim(out_io_msg)
     error stop "Unable to open output file"
  end if

  if (verbosity >= 10)write (output_unit, '(a40)') "Running Simulations"
  do tmp_j=1,num_runs
     call main_sim(tmp_j)
     if ((verbosity >= 10) .and. (mod(tmp_j, max(1_ik, num_runs/10_ik)) == 0)) write (output_unit, '(a40,i20)') "runs complete:", tmp_j
  end do
  if (verbosity >= 10)write (output_unit, '(a40)') "Simulations Complete"

  close(unit=out_io_unit, status='keep', iostat=out_io_stat, iomsg=out_io_msg)
  if (out_io_stat /= 0) then
     write (error_unit, '(a)') trim(out_io_msg)
     error stop "I/O error closing output file"
  end if

  call cpu_time(cpu_time_end)

  if (verbosity >= 10)write (output_unit, '(a40,f20.1)') "CPU Time (s):", (cpu_time_end-cpu_time_start)

contains

  !------------------------------------------------------------------------------------------------------------------------------
  subroutine main_sim(sim)
    implicit none
    integer(kind=ik), intent(in) :: sim

     real(kind=rk)     :: cur_std_tax_deduction_single, cur_std_tax_deduction_joint, social_security_monthly_p1
     real(kind=rk)     :: social_security_monthly_p2, gross_work_salary_p2, gross_work_salary_p1, annual_ira_contrib_base
     real(kind=rk)     :: annual_ira_contrib_catchup, annual_roth_contrib_base, annual_roth_contrib_catchup
     real(kind=rk)     :: cur_annual_roth_contrib_growth, ira_savings_p1, ira_savings_p2, gross_work_income_p1
     real(kind=rk)     :: gross_work_income_p2, work_income_p1, work_income_p2, ss_income_p1, ss_income_p2, roth_savings_p1
     real(kind=rk)     :: roth_savings_p2, taxable_income, tax_rate, tax_owed, cur_inflation_rate, cur_emergency_fund_growth
     real(kind=rk)     :: cur_work_salary_growth, cur_social_security_growth, cur_annual_ira_contrib_growth
     real(kind=rk)     :: cur_investment_apr(3), cur_investment_mix(3), cr_paied_cash, cr_paied_savings, cr_paied_ira
     real(kind=rk)     :: cr_paied_roth, annual_expenses_paied_cash, annual_expenses_paied_savings, annual_expenses_paied_roth
     real(kind=rk)     :: annual_expenses_paied_ira, tax_paied_cash, tax_paied_savings, tax_paied_ira, tax_paied_roth
     real(kind=rk)     :: start_cash_income, roth_convert_p2, roth_convert_p1
     real(kind=rk)     :: cur_tax_bracket_breaks_single(size(tax_bracket_breaks_single))
     real(kind=rk)     :: cur_tax_bracket_breaks_joint(size(tax_bracket_breaks_joint))
     integer(kind=ik)  :: mc_year, death_p1, death_p2
     character(len=1)  :: status_p1, status_p2

     !                                         s   y a1 a2    S1/S2      cash       inf       CpI/ST/SI/SR  ef B I12 R12  apr          roth con1/2  ss1/2 wrk1/2 sav1 sav2 exp epI/T/I/R  taxbl      bkt       tax tpI/T/SI/SR
     character(len=*), parameter  :: fmt_n = "(i7, 3(1x, i4), 2(1x, a2), 1x, f12.2, 1x, f5.1, 4(1x, f10.2), 6(1x, f16.2), 3(1x, f6.1), 2(1x, f11.2), 4(1x, f9.2), 2(1x, f8.2), 5(1x, f11.2), 1x, f14.2, 1x, f6.2, 5(1x, f14.2))"
     character(len=*), parameter  :: fmt_h = "(a7, 3(1x, a4), 2(1x, a2), 1x, a12,   1x, a5,   4(1x, a10),   6(1x, a16),   3(1x, a6),   2(1x, a11),   4(1x, a9),   2(1x, a8),   5(1x, a11),   1x, a14,   1x, a6,   5(1x, a14) )"

     if (sim == 1) then
        write (unit=out_io_unit, iostat=out_io_stat, iomsg=out_io_msg, fmt=fmt_h) &
             "Sim", "Year", "Age1", "Age2", "S1", "S2", &
             "SavingsC", "Inf", "CPaidI", "CPaidST", "CPaidSI", "CPaidSR", &
             "SavingsE", "SavingsB", "SavingsI1", "SavingsI2", "SavingsR1", "SavingsR2", "aprH", "arpM", "aprL", &
             "ConR1", "ConR2", &
             "SS1", "SS2", "Wrk1", "Wrk2", &
             "Sav1", "Sav2", &
             "Expenses", "EPaidI", "EPaidST", "EPaidSI", "EPaidSR", &
             "Taxable", "Bkt", "Taxes", "TPaidI", "TPaidST", "TPaidSI", "TPaidSR"
        if (out_io_stat > 0) then
           write (error_unit, '(a)') trim(out_io_msg)
           error stop "I/O Error output file"
        end if
     end if

     ! -----------------------------------------------------------------------------------------------------------------------------
     ! Main simulation loop initialization & loop
     brokerage_balance             = initial_brokerage_balance
     ira_balance_p2                = initial_ira_balance_p2
     ira_balance_p1                = initial_ira_balance_p1
     roth_balance_p2               = initial_roth_balance_p2
     roth_balance_p1               = initial_roth_balance_p1
     expected_expenses_shr         = initial_expected_expenses_shr
     expected_expenses_p1          = initial_expected_expenses_p1
     expected_expenses_p2          = initial_expected_expenses_p2
     cur_std_tax_deduction_single  = std_tax_deduction_single
     cur_std_tax_deduction_joint   = std_tax_deduction_joint
     social_security_monthly_p1    = initial_social_security_monthly_p1
     social_security_monthly_p2    = initial_social_security_monthly_p2
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
     total_expected_expenses       = -1.0
     last_roth_conversion_year_p1  = simulation_year_start - 5_ik
     last_roth_conversion_year_p2  = simulation_year_start - 5_ik
     cur_investment_mix            = [ high_investment_p, mid_investment_p, low_investment_p ]
     death_p1                      = life_expectancy_p1
     death_p2                      = life_expectancy_p2
     simulation_year_end           = -1

     if (death_p1 < 0) then
        if (sex_p1 == "M") then
           death_p1 = rand_age(simulation_year_start-birthday_p1, usss_m_lx_dat, 0_ik)
        else
           death_p1 = rand_age(simulation_year_start-birthday_p1, usss_f_lx_dat, 0_ik)
        end if
     end if
     if (death_p2 < 0) then
        if (sex_p2 == "M") then
           death_p2 = rand_age(simulation_year_start-birthday_p2, usss_m_lx_dat, 0_ik)
        else
           death_p2 = rand_age(simulation_year_start-birthday_p2, usss_f_lx_dat, 0_ik)
        end if
     end if

     simulation_year_end           = max(birthday_p1+death_p1, birthday_p2+death_p2)

     do year=simulation_year_start, simulation_year_end

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Figure out ages
        age_p1 = year - birthday_p1
        age_p2 = year - birthday_p2

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Figure out status
        if (age_p1 < death_p1) then
           if (year < retirement_year_p1) then
              status_p1 = 'W'
           else
              status_p1 = 'R'
           end if
        else
           status_p1 = 'D'
        end if
        if (age_p2 < death_p2) then
           if (year < retirement_year_p2) then
              status_p2 = 'W'
           else
              status_p2 = 'R'
           end if
        else
           status_p2 = 'D'
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! We always do MC even when num_runs==1..
        mc_year = rand_int(mc_year_high, mc_year_low)

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Total expenses
        total_expected_expenses  = expected_expenses_shr
        if (age_p1 < death_p1) then
           total_expected_expenses = total_expected_expenses + expected_expenses_p1
        end if
        if (age_p2 < death_p2) then
           total_expected_expenses = total_expected_expenses + expected_expenses_p2
        end if
        ! ------------------------------------------------------------------------------------------------------------------------
        ! Fix a value for investments (invest or use cash)
        if ((worst_case_inflation_rate < 0) .or. &
             (cash_reserves + emergency_fund + brokerage_balance + ira_balance_p1 + ira_balance_p2 + roth_balance_p1 + roth_balance_p2 < &
             tvm_geometric_annuity_sum_a(1_ik+simulation_year_end-year, worst_case_inflation_rate, total_expected_expenses))) then
           cur_investment_apr(1)  = alt_if_neg(high_investment_apr, snp_dat(mc_year))
           cur_investment_apr(2)  = alt_if_neg(mid_investment_apr,  cur_investment_apr(1)/2)
           cur_investment_apr(3)  = alt_if_neg(low_investment_apr,  dgs10_dat(mc_year))
        else
           ! we have so much money at this point we don't need to aggressively invest
           cur_investment_apr  = cash_position_growth
        end if

        cur_emergency_fund_growth        = alt_if_neg(emergency_fund_growth,      cur_investment_apr(3))
        cur_inflation_rate               = alt_if_neg(fixed_inflation_rate,       inf_dat(mc_year))
        cur_work_salary_growth           = alt_if_neg(work_salary_growth,         max(0.0_rk, cur_inflation_rate/2))
        cur_social_security_growth       = alt_if_neg(social_security_growth,     max(0.0_rk, cur_inflation_rate))
        cur_annual_ira_contrib_growth    = alt_if_neg(annual_ira_contrib_growth,  max(0.0_rk, cur_inflation_rate))
        cur_annual_roth_contrib_growth   = alt_if_neg(annual_roth_contrib_growth, max(0.0_rk, cur_inflation_rate))

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
        if ((age_p1 >= social_security_start_age_p1) .and. (age_p1 < death_p1)) then
           ss_income_p1 = social_security_monthly_p1 * 12
        end if
        ss_income_p2 = 0
        if ((age_p2 >= social_security_start_age_p2) .and. (age_p2 < death_p2)) then
           ss_income_p2 = social_security_monthly_p2 * 12
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Total income from non-savings sources
        cash_income = ss_income_p1 + ss_income_p2 + work_income_p1 + work_income_p2
        start_cash_income = cash_income

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Initialize taxable income.  It will be updated later for retirement account withdrawals
        taxable_income = cash_income                                                                                 ! Taxable Non-savings income
        taxable_income = taxable_income + sum(p_of(p_of(brokerage_balance, cur_investment_mix), cur_investment_apr)) ! Taxable investment income
        taxable_income = taxable_income + p_of(emergency_fund, emergency_fund_growth)                                ! Taxable investment income
        taxable_income = taxable_income + p_of(cash_reserves, cash_reserves_growth)                                  ! Taxable investment income

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Pay annual expenses
        call pay_stuff(total_expected_expenses, annual_expenses_paied_cash, annual_expenses_paied_savings, &
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
        ! Roth Conversion
        roth_convert_p2 = 0
        roth_convert_p1 = 0
        if (year < maximum_roth_conversion_year) then
           if (target_taxable_income - taxable_income > minimum_roth_conversion ) then
              if (ira_balance_p2 > 0) then
                 roth_convert_p2 = min(ira_balance_p2, target_taxable_income-taxable_income)
                 roth_balance_p2 = roth_balance_p2 + roth_convert_p2
                 ira_balance_p2  = ira_balance_p2  - roth_convert_p2
                 taxable_income  = taxable_income  + roth_convert_p2
                 last_roth_conversion_year_p2 = year
              end if
           end if
           if (target_taxable_income - taxable_income > minimum_roth_conversion ) then
              if (ira_balance_p1 > 0) then
                 roth_convert_p1 = min(ira_balance_p1, target_taxable_income-taxable_income)
                 roth_balance_p1 = roth_balance_p1 + roth_convert_p1
                 ira_balance_p1  = ira_balance_p1  - roth_convert_p1
                 taxable_income  = taxable_income  + roth_convert_p1
                 last_roth_conversion_year_p1 = year
              end if
           end if
        end if

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
        if ((age_p1 < death_p1) .and. (age_p2 < death_p2)) then
           taxable_income = taxable_income - cur_std_tax_deduction_joint
        else
           taxable_income = taxable_income - cur_std_tax_deduction_single
        end if
        if (taxable_income < 0) then
           taxable_income = 0
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Print status to output file
        write (unit=out_io_unit, iostat=out_io_stat, iomsg=out_io_msg, fmt=fmt_n) &
             sim, year, age_p1, age_p2, status_p1, status_p2, &               
             cash_reserves, cur_inflation_rate, cr_paied_cash, cr_paied_savings, cr_paied_ira, cr_paied_roth, &
             emergency_fund, brokerage_balance, ira_balance_p1, ira_balance_p2,roth_balance_p1, roth_balance_p2, &
             cur_investment_apr(1), cur_investment_apr(2), cur_investment_apr(3), &
             roth_convert_p1, roth_convert_p2, &
             ss_income_p1, ss_income_p2, gross_work_income_p1, gross_work_income_p2, &
             ira_savings_p1, ira_savings_p2, &
             total_expected_expenses, annual_expenses_paied_cash, annual_expenses_paied_savings, &
             annual_expenses_paied_ira, annual_expenses_paied_roth, &
             taxable_income, tax_rate, tax_owed, tax_paied_cash, tax_paied_savings, tax_paied_ira, tax_paied_roth
        if (out_io_stat > 0) then
           write (error_unit, '(a)') trim(out_io_msg)
           error stop "I/O Error output file"
        end if

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Compute Taxes For Next Year
        if ((age_p1 < death_p1) .and. (age_p2 < death_p2)) then
           tax_owed = tax(taxable_income, cur_tax_bracket_breaks_joint, tax_bracket_rates)
        else
           tax_owed = tax(taxable_income, cur_tax_bracket_breaks_single, tax_bracket_rates)
        end if
        tax_rate = percentage_of_total(taxable_income, tax_owed)

        ! ------------------------------------------------------------------------------------------------------------------------
        ! Grow values for inflation, wage growth, ss growth, etc...
        expected_expenses_shr         = add_p(expected_expenses_shr,         cur_inflation_rate)             ! Inflation
        expected_expenses_p1          = add_p(expected_expenses_p1,          cur_inflation_rate)             ! Inflation
        expected_expenses_p2          = add_p(expected_expenses_p2,          cur_inflation_rate)             ! Inflation
        target_taxable_income         = add_p(target_taxable_income,         cur_inflation_rate)             ! Inflation
        gross_work_salary_p2          = add_p(gross_work_salary_p2,          cur_work_salary_growth)         ! Raise at work
        gross_work_salary_p1          = add_p(gross_work_salary_p1,          cur_work_salary_growth)         ! Raise at work
        social_security_monthly_p1    = add_p(social_security_monthly_p1,    cur_social_security_growth)     ! Raise for SS
        social_security_monthly_p2    = add_p(social_security_monthly_p2,    cur_social_security_growth)     ! Raise for SS
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
   integer(kind=ik) function alt_if_neg_i(val1, val2)
     integer(kind=ik), intent(in) :: val1
     integer(kind=ik), intent(in) :: val2
     if (val1 < 0) then
        alt_if_neg_i = val2
     else
        alt_if_neg_i = val1
     end if
   end function alt_if_neg_i

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
     if ((paid < bill) .and. (roth_balance_p1 > 0) .and. (60 <= age_p1) .and. (year > last_roth_conversion_year_p1+5)) then       ! Use roth_balance_p1
        ded = min(bill - paid, roth_balance_p1)
        paid = paid + ded
        paid_from_roth = paid_from_roth + ded
        roth_balance_p1 = roth_balance_p1 - ded
     end if
     if ((paid < bill) .and. (roth_balance_p2 > 0) .and. (60 <= age_p2).and. (year > last_roth_conversion_year_p2+5)) then       ! Use roth_balance_p2
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
     namelist /SIMPARM/ initial_expected_expenses_shr, initial_expected_expenses_p1, initial_expected_expenses_p2
     namelist /SIMPARM/ social_security_start_age_p1, social_security_start_age_p2
     namelist /SIMPARM/ initial_social_security_monthly_p1, initial_social_security_monthly_p2, social_security_growth
     namelist /SIMPARM/ initial_gross_work_salary_p1, initial_gross_work_salary_p2, work_salary_growth
     namelist /SIMPARM/ initial_annual_ira_contrib_base, initial_annual_ira_contrib_catchup, annual_ira_contrib_growth
     namelist /SIMPARM/ target_taxable_income, minimum_roth_conversion, maximum_roth_conversion_year
     namelist /SIMPARM/ surplus_reinvest
     namelist /SIMPARM/ retirement_year_p1, retirement_year_p2
     namelist /SIMPARM/ birthday_p1, birthday_p2, life_expectancy_p1, life_expectancy_p2, sex_p1, sex_p2
     namelist /SIMPARM/ verbosity

     ! Variables for config file
     integer                       :: in_io_stat, in_io_unit, in_file_name_len
     character(len=512)            :: in_io_msg
     character(len=:), allocatable :: in_file_name

     call get_command(length=in_file_name_len)
     allocate(character(len=in_file_name_len) :: in_file_name)
     call get_command_argument(1, in_file_name, in_file_name_len)
     in_file_name = adjustl(trim(in_file_name))

     if (len(in_file_name) <= 0) then
        in_file_name = "retire.nml"
     end if

     open(newunit=in_io_unit, file=in_file_name, form='formatted', action='read', iostat=in_io_stat, iomsg=in_io_msg)
     if (in_io_stat /= 0) then
        write (error_unit, '(a)') trim(in_io_msg)
        error stop "Unable to open config file"
     end if
     read (nml=SIMPARM, unit=in_io_unit, iostat=in_io_stat, iomsg=in_io_msg)
     if (in_io_stat > 0) then
        write (error_unit, '(a)') trim(in_io_msg)
        error stop "I/O Error reading configuration"
     end if
     close(unit=in_io_unit, status='keep', iostat=in_io_stat, iomsg=in_io_msg)
     if (in_io_stat /= 0) then
        write (error_unit, '(a)') trim(in_io_msg)
        error stop "I/O error closing file"
     end if

   end subroutine read_config

 end program retire

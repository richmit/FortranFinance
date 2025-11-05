! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_cashflows.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Time value of money for general cashflows.@EOL
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
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.E.!!

!----------------------------------------------------------------------------------------------------------------------------------
!> Tools for TVM computations with irregular/uneven cashflows.
!!
!! The traditional definition of a cashflow is money received (positive) or paid (negative).  A cashflow stream or series (or
!! sequence) is one or more cashflows received over a period of time.  Time is broken up into a series of discreet "periods"
!! corresponding naturally to the problem at hand. Each cashflow occurs at the beginning or ending of one of these periods.  For
!! example, we might break time up into months to model the cashflows related to a loan.  Software approaches vary with regard to
!! how cashflows are entered, stored, and processed.
!!
!! In this library the time of a cashflow is specified by the period boundary on which it occurs.  We measure time in units of
!! periods with the beginning of the first period begin time 0.  An n period cashflow is stored in a simple array (or a column of
!! a matrix) with capacity for n+1 elements (one for each boundary) -- note that is one more than the number of periods.  The
!! first element of a cash flow array represents the first boundary at time *zero* and is the *beginning* of time period 1.  The
!! second element of the array represents the *end* of time period 1 or the *beginning* of time period 2.  The last element
!! represents the final boundary at time 6 at *end* of the final period (period n).
!!
!! Consider an example.  We get a loan for 1000 over 6 months with a monthly interest rate of 1%.  This is a 6 period cash flow
!! with the principal received at the beginning of period 1.  Conventionally we think of the first payment occurring at the end
!! of period 1; however, we could think of this payment as occurring at the beginning of period 2. For this example, the cashflow
!! array would be: [1000, -172.55, -172.55, -172.55, -172.55, -172.55, -172.55].  In tabular form:
!!
!! @verbatim
!!                          Array Index   Cashflow
!!                                    1    1000.00   ==> Time 0 (start of 1st period)
!!                                    2    -172.55   ==> Time 1 (end of 1st period/start of 2nd period)
!!                                    3    -172.55
!!                                    4    -172.55
!!                                    5    -172.55
!!                                    6    -172.55
!!                                    7    -172.55   ==> Time 6 (end of 6th period)
!! @endverbatim
!!
!! This library encourages the use of multiple cashflow series for problem solving.  Each cashflow sequence is stored as the
!! column of a matrix.  The entire matrix may then be used for TVM calculations.
!!
module mrffl_cashflows
  use :: mrffl_config,      only: rk, fvfmt_ai, ftfmt_ai, zero_epsilon
  use :: mrffl_bitset,      only: bitset_subsetp, bitset_not_subsetp, bitset_intersectp, bitset_minus
  use :: mrffl_percentages, only: percentage_to_fraction
  use :: mrffl_prt_sets,    only: prt_NONE, prt_param, prt_title, prt_table, prt_total, prt_space, prt_fv, prt_fv_agg_val, prt_fv_agg_sum, prt_pv, prt_pv_agg_val, prt_pv_agg_sum, prt_cf, prt_cf_agg_val, prt_cf_agg_sum, prt_ALL
  use :: mrffl_solver,      only: multi_bisection
  implicit none (type, external)
  private

  ! Work with one or more cashflow series in the columns of a matrix
  public :: cashflow_matrix_cmp, cashflow_matrix_total_pv, cashflow_matrix_irr

  ! Work with one cashflow series in a vector
  public :: cashflow_vector_cmp, cashflow_vector_total_pv, cashflow_vector_irr

  ! Construct cashflow series
  public :: make_cashflow_vector_delayed_lump, make_cashflow_vector_delayed_level_annuity
  public :: make_cashflow_vector_delayed_geometric_annuity, make_cashflow_vector_delayed_arithmetic_annuity

  ! Modify cashflow series
  public :: add_intrest_to_cashflow_vector, add_multi_intrest_to_cashflow_vector

  ! interface cashflow_total_pv
  !    module procedure cashflow_matrix_total_pv, cashflow_vector_total_pv
  ! end interface cashflow_total_pv
  ! public :: cashflow_total_pv

contains

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute pv for a cashflow vector.
  !!
  !! See: cashflow_matrix_total_pv()
  !!
  real(kind=rk) pure function cashflow_vector_total_pv(cf_vec, i)
    ! Arguments
    real(kind=rk), intent(in)  :: cf_vec(:)
    real(kind=rk), intent(in)  :: i
    ! Local Variables
    integer :: j
    cashflow_vector_total_pv = 0
    do j=1,size(cf_vec)
       cashflow_vector_total_pv = cashflow_vector_total_pv + cf_vec(j) / (1+percentage_to_fraction(i))**(j-1)
    end do
  end function cashflow_vector_total_pv

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute pv for a cashflow matrix.
  !!
  !! In this library, initial cashflows are simply at time 0.  NPV and PV are the same value in this context.  The value
  !! returned by this function is identical to summing the pv_vec returned by cashflow_matrix_cmp; however, this function is
  !! much faster and requires no temporary arrays.
  !!
  !! @param cf_mat    Matrix of cashflows (one cashflow sequence per column)
  !! @param i         Interest/Rate/Growth
  !!
  real(kind=rk) pure function cashflow_matrix_total_pv(cf_mat, i)
    ! Arguments
    real(kind=rk), intent(in)  :: cf_mat(:,:)
    real(kind=rk), intent(in)  :: i
    ! Local Variables
    real(kind=rk) :: cf
    integer       :: j, k
    ! Perform Computation
    cashflow_matrix_total_pv = 0
    do j=1,size(cf_mat, 1)
       cf = cf_mat(j, 1)
       do k=2,size(cf_mat, 2)
          cf = cf + cf_mat(j, k)
       end do
       cashflow_matrix_total_pv = cashflow_matrix_total_pv + cf / (1+percentage_to_fraction(i))**(j-1)
    end do
  end function cashflow_matrix_total_pv

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute IRR for a cashflow vector.
  !!
  !! @param cf_vec    Vector of cashflows
  !! @param irr       If the solver is successful, this will be the irr on return.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2161-2193.
  !!
  subroutine cashflow_vector_irr(cf_vec, irr, status)
    ! Arguments
    real(kind=rk), intent(in)    :: cf_vec(:)
    real(kind=rk), intent(inout) :: irr
    integer,       intent(out)   :: status
    ! Local Variables
    real(kind=rk), parameter :: islvivl0(3) = [0.0_rk+zero_epsilon, -100.0_rk+zero_epsilon,            -99999.0_rk]
    real(kind=rk), parameter :: islvivl1(3) = [         99999.0_rk,    0.0_rk-zero_epsilon, -100.0_rk-zero_epsilon]
    ! Perform Computation
    call multi_bisection(irr, islvivl0, islvivl1, irr_solve, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
    if (status /= 0) then
       status = 2161 ! "ERROR(cashflow_vector_irr): irr solver failed!"
    end if
    return
  contains
    real(kind=rk) function irr_solve(i)
      real(kind=rk), intent(in) :: i
      irr_solve = cashflow_vector_total_pv(cf_vec, i)
    end function irr_solve
  end subroutine cashflow_vector_irr

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute IRR for a cashflow matrix.
  !!
  !! @param cf_mat    Matrix of cashflows (one cashflow sequence per column)
  !! @param irr       If the solver is successful, this will be the irr on return.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2194-2226.
  !!
  subroutine cashflow_matrix_irr(cf_mat, irr, status)
    ! Arguments
    real(kind=rk), intent(in)    :: cf_mat(:,:)
    real(kind=rk), intent(inout) :: irr
    integer,       intent(out)   :: status
    ! Local Variables
    real(kind=rk), parameter :: islvivl0(3) = [0.0_rk+zero_epsilon, -100.0_rk+zero_epsilon,            -99999.0_rk]
    real(kind=rk), parameter :: islvivl1(3) = [         99999.0_rk,    0.0_rk-zero_epsilon, -100.0_rk-zero_epsilon]
    ! Perform Computation
    call multi_bisection(irr, islvivl0, islvivl1, irr_solve, 1.0e-5_rk, 1.0e-5_rk, 1000, status, .false.)
    if (status /= 0) then
       status = 2194 ! "ERROR(cashflow_matrix_irr): irr solver failed!"
    end if
    return
  contains
    real(kind=rk) function irr_solve(i)
      real(kind=rk), intent(in) :: i
      irr_solve = cashflow_matrix_total_pv(cf_mat, i)
    end function irr_solve
  end subroutine cashflow_matrix_irr

  !------------------------------------------------------------------------------------------------------------------------------
  !> Convert a cashflow number into a padded string for titles
  !!
  character(len=5) function i2s(n)
    !
    implicit none (type, external)
    ! Local Variables
    integer, intent(in) :: n
    ! Perform Computation
    write(i2s,'(i5.5)') n
  end function i2s

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute present and future values for a cashflow vector.
  !!
  !! See: cashflow_matrix_cmp()
  !!
  subroutine cashflow_vector_cmp(status, cf_vec, i, pv_vec, fv_vec, prt_o, fvfmt_o, ftfmt_o)
    ! Arguments
    real(kind=rk),              intent(in)  :: cf_vec(:)
    real(kind=rk),              intent(in)  :: i
    real(kind=rk),              intent(out) :: pv_vec(:), fv_vec(:)
    integer,                    intent(out) :: status
    integer,          optional, intent(in)  :: prt_o
    character(len=*), optional, intent(in)  :: fvfmt_o, ftfmt_o
    ! Local Variables
    integer                       :: prt
    character(len=:), allocatable :: fvfmt, ftfmt
    ! Process Optional Arguments
    prt = prt_NONE
    if (present(prt_o)) prt = prt_o
    fvfmt = fvfmt_ai
    if (present(fvfmt_o)) fvfmt = fvfmt_o
    ftfmt = ftfmt_ai
    if (present(ftfmt_o)) ftfmt = ftfmt_o
    ! Perform Computation
    call cashflow_matrix_cmp(status, reshape(cf_vec, [size(cf_vec), 1]), i, pv_agg_o=pv_vec, fv_agg_o=fv_vec, prt_o=prt, fvfmt_o=fvfmt, ftfmt_o=ftfmt)
  end subroutine cashflow_vector_cmp

  !------------------------------------------------------------------------------------------------------------------------------
  !> Compute present and future values for a cashflow matrix.
  !!
  !! As a side effect, the cashflows may be printed.
  !!
  !! @param status    Returns status of operation.  0 if everything worked. Range: 0 & 2129-2160.
  !! @param cf        Matrix of cashflows (one cashflow sequence per column)
  !! @param i         Interest/Rate/Growth
  !! @param pv_agg_o  Returns the summary/aggregate 'present value' vector.  Default: NONE
  !! @param fv_agg_o  Returns the summary/aggregate 'future value' vector.  Default: NONE
  !! @param pv_o      Returns the 'present value' matrix with a column for each cashflow sequence.  Default: NONE
  !! @param fv_o      Returns the 'future value' matrix with a column for each cashflow sequence.  Default: NONE
  !! @param prt_o     Bitset built from the following constants prt_param, prt_title, prt_table, prt_total, & prt_space
  !!                   - Print requests for items not computed will be silglentedly ignored.  
  !!                      - prt_cf_* requires one of pv_agg_o or fv_agg_o. 
  !!                      - prt_pv_* requires pv_agg_o
  !!                      - prt_fv_* requires fv_agg_o.  
  !!                      - prt_pv requires pv_o, and prt_fv requires fv_o.
  !!                   - If flows==1, then prt_*_agg is ignored if the matching prt_* is set.
  !! @param fvfmt_o   Floating point value output format.  Default: fvfmt_ai
  !! @param ftfmt_o   Floating point tital output format.  Default: ftfmt_ai
  !!
  subroutine cashflow_matrix_cmp(status, cf, i, pv_o, pv_agg_o, fv_o, fv_agg_o, prt_o, fvfmt_o, ftfmt_o)
    implicit none (type, external)
    ! Arguments
    real(kind=rk),              intent(in)  :: cf(:,:)
    real(kind=rk),              intent(in)  :: i
    integer,                    intent(out) :: status
    real(kind=rk),    optional, intent(out) :: pv_o(:,:), fv_o(:,:)
    real(kind=rk),    optional, intent(out) :: pv_agg_o(:), fv_agg_o(:)
    integer,          optional, intent(in)  :: prt_o
    character(len=*), optional, intent(in)  :: fvfmt_o, ftfmt_o
    ! Local Variables
    integer                       :: num_bdrys, num_flows, j, flow, prt
    character(len=:), allocatable :: fvfmt, ftfmt
    real(kind=rk),    allocatable :: dfactors(:), cf_agg(:)
    ! Process Optional Arguments
    prt = prt_NONE
    if (present(prt_o)) prt = prt_o
    fvfmt = fvfmt_ai
    if (present(fvfmt_o)) fvfmt = fvfmt_o
    ftfmt = ftfmt_ai
    if (present(ftfmt_o)) ftfmt = ftfmt_o
    ! Process & Check Arguments
    num_bdrys = size(cf, 1)
    num_flows = size(cf, 2)
    if (num_flows < 1) then
       status = 2130 ! "ERROR(cashflow_matrix_cmp): No flows found in matrix!"
       return
    end if
    if (abs(i+100) < zero_epsilon) then
       status = 2131 ! "ERROR(cashflow_matrix_cmp): Value for i is too close -100%!"
       return
    end if
    if (num_bdrys < 2) then
       status = 2132 ! "ERROR(cashflow_matrix_cmp): Number of periods in cashflow is too small!"
       return
    end if
    if (present(pv_agg_o)) then
       if (num_bdrys > size(pv_agg_o)) then
          status = 2133 ! "ERROR(cashflow_matrix_cmp): The pv_agg_o array is not long enough!"
          return
       end if
    else
       prt = bitset_minus(prt, prt_pv_agg_val)
       prt = bitset_minus(prt, prt_pv_agg_sum)
    end if
    if (present(fv_agg_o)) then
       if (num_bdrys > size(fv_agg_o)) then
          status = 2134 ! "ERROR(cashflow_matrix_cmp): The fv_agg_o array is not long enough!"
          return
       end if
    else
       prt = bitset_minus(prt, prt_fv_agg_val)
       prt = bitset_minus(prt, prt_fv_agg_sum)
    end if
    if ( .not. (present(fv_agg_o) .or. present(pv_agg_o))) then
       prt = bitset_minus(prt, prt_cf_agg_val)
       prt = bitset_minus(prt, prt_cf_agg_sum)
    end if
    if (present(pv_o)) then
       if (num_bdrys > size(pv_o, 1)) then
          status = 2136 ! "ERROR(cashflow_matrix_cmp): The pv_o array is not long enough!"
          return
       end if
       if (size(cf, 2) > size(pv_o, 2)) then
          status = 2137 ! "ERROR(cashflow_matrix_cmp): The pv_o array is not wide enough!"
          return
       end if
    else
       prt = bitset_minus(prt, prt_pv)
    end if
    if (present(fv_o)) then
       if (num_bdrys > size(fv_o, 1)) then
          status = 2138 ! "ERROR(cashflow_matrix_cmp): The fv_o array is not long enough!"
          return
       end if
       if (size(cf, 2) > size(fv_o, 2)) then
          status = 2139 ! "ERROR(cashflow_matrix_cmp): The fv_o array is not wide enough!"
          return
       end if
    else
       prt = bitset_minus(prt, prt_fv)
    end if
    ! Suppress columns with duplicate data
    if ((num_flows == 1) .and. bitset_subsetp(prt_cf_agg_val+prt_cf, prt)) then
       prt = bitset_minus(prt, prt_cf_agg_val)
    end if
    if ((num_flows == 1) .and. bitset_subsetp(prt_pv_agg_val+prt_pv, prt)) then
       prt = bitset_minus(prt, prt_pv_agg_val)
    end if
    if ((num_flows == 1) .and. bitset_subsetp(prt_fv_agg_val+prt_fv, prt)) then
       prt = bitset_minus(prt, prt_fv_agg_val)
    end if
    ! If we have nothing to print in a table or total...
    if (.not. bitset_intersectp(prt, prt_fv+prt_fv_agg_val+prt_fv_agg_sum+ &
         &                           prt_pv+prt_pv_agg_val+prt_pv_agg_sum+ &
         &                           prt_cf+prt_cf_agg_val+prt_cf_agg_sum)) then
       prt = bitset_minus(prt, prt_table+prt_total)
    end if
    ! Args look good
    status = 0
    ! Perform Computation
    if (abs(i+100) < zero_epsilon) then
       dfactors = [(1,j=1,num_bdrys)]
    else
       dfactors = (1.0_rk+percentage_to_fraction(i))**[(j-1,j=1,num_bdrys)]
    end if
    ! Compute the vector values
    if (present(pv_agg_o) .or. present(fv_agg_o)) then
       cf_agg = sum(cf, 2)
       if (present(pv_agg_o)) pv_agg_o = cf_agg / dfactors
       if (present(fv_agg_o)) fv_agg_o = cf_agg * dfactors(num_bdrys:1:-1)
    else
       cf_agg = [0] ! Suppress unbound/uninitialized warning
    end if
    ! Compute the matrix values
    if (present(pv_o)) then
       do flow=1, num_flows
          pv_o(:, flow) = cf(:, flow) / dfactors
       end do
    end if
    if (present(fv_o)) then
       do flow=1, num_flows
          fv_o(:, flow) = cf(:, flow) * dfactors(num_bdrys:1:-1)
       end do
    end if
    ! Print stuff
    if (bitset_subsetp(prt_space, prt) .and. bitset_intersectp(prt_param+prt_table+prt_total, prt)) then
       write (*, *) ""
    end if
    if (bitset_intersectp(prt_param+prt_table+prt_total+prt_space, prt)) then
       if (bitset_subsetp(prt_param, prt)) then
          write (*, fmt="(a15, i25)")   "Period Count: ",  (num_bdrys-1)
          write (*, fmt="(a15, f25.4)") "Discount Rate: ", i
          if (bitset_subsetp(prt_space, prt) .and. bitset_intersectp(prt_table+prt_total, prt)) then
             write (*, *) ""
          end if
       end if
       if (bitset_subsetp(prt_title, prt)) then
          if (bitset_intersectp(prt_table+prt_total, prt)) then
             if (bitset_subsetp(prt_table, prt)) then
                write (*, fmt="(a6)",    advance='no')  "Time"
             else
                write (*, fmt="(a6)",    advance='no')  ""
             end if
             if (bitset_subsetp(prt_cf,         prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  ("CF_"//i2s(flow), flow = 1, num_flows )
             if (bitset_subsetp(prt_cf_agg_val, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "CF_Aggregate"
             if (bitset_subsetp(prt_pv,         prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  ("PV_"//i2s(flow), flow = 1, num_flows )
             if (bitset_subsetp(prt_pv_agg_val, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "PV_Aggregate"
             if (bitset_subsetp(prt_fv,         prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  ("FV_"//i2s(flow), flow = 1, num_flows )
             if (bitset_subsetp(prt_fv_agg_val, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "FV_Aggregate"
             if (bitset_subsetp(prt_table, prt)) then
                if (bitset_subsetp(prt_cf_agg_sum, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "CF_Sum"
                if (bitset_subsetp(prt_pv_agg_sum, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "PV_Sum"
                if (bitset_subsetp(prt_fv_agg_sum, prt)) write (*, fmt="(*("//ftfmt//"))", advance='no')  "FV_Sum"
             end if
             write (*, *) ""
          end if
       end if
       if (bitset_subsetp(prt_table, prt)) then
          do j = 1, num_bdrys
             write (*, fmt="(i6)",    advance='no')  (j-1)
             if (bitset_subsetp(prt_cf,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  cf(j, :)
             if (bitset_subsetp(prt_cf_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  cf_agg(j)
             if (bitset_subsetp(prt_pv,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  pv_o(j, :)
             if (bitset_subsetp(prt_pv_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  pv_agg_o(j)
             if (bitset_subsetp(prt_fv,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  fv_o(j, :)
             if (bitset_subsetp(prt_fv_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  fv_agg_o(j)
             if (bitset_subsetp(prt_cf_agg_sum, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(cf_agg(1:j))
             if (bitset_subsetp(prt_pv_agg_sum, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(pv_agg_o(1:j))
             if (bitset_subsetp(prt_fv_agg_sum, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(fv_agg_o(1:j))
             write (*, *) ""
          end do
          if (bitset_subsetp(prt_space+prt_total, prt)) then
             write (*, *) ""
          end if
       end if
       if (bitset_subsetp(prt_total, prt)) then
          write (*, fmt="(a6)",    advance='no')  "TOTAL"
          if (bitset_subsetp(prt_cf,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(cf, 1)
          if (bitset_subsetp(prt_cf_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(cf_agg)
          if (bitset_subsetp(prt_pv,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(pv_o, 1)
          if (bitset_subsetp(prt_pv_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(pv_agg_o)
          if (bitset_subsetp(prt_fv,         prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(fv_o, 1)
          if (bitset_subsetp(prt_fv_agg_val, prt)) write (*, fmt="(*("//fvfmt//"))", advance='no')  sum(fv_agg_o)
          write (*, *) ""
       end if
       if (bitset_subsetp(prt_space, prt) .and. bitset_intersectp(prt_param+prt_table+prt_total, prt)) then
          write (*, *) ""
       end if
    end if
  end subroutine cashflow_matrix_cmp

  !------------------------------------------------------------------------------------------------------------------------------
  !> Create a cashflow with a single (lump sum) payment.
  !!
  !! The number of periods is assumed from the size of the cashflow vector at size(cashflow)-1.
  !! All payments other than the lump sum payment are set to zero.
  !!
  !! @param cf_vec    The resulting cashflow vector.
  !! @param a         The cashflow amount.
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2097-2128.
  !!
  subroutine make_cashflow_vector_delayed_lump(cf_vec, a, d, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: a
    integer,       intent(in)  :: d
    integer,       intent(out) :: status
    ! Local Variables
    integer :: n
    ! Perform Computation
    n = size(cf_vec)-1
    if (n < 1) then
       status = 2097 ! "ERROR(make_cashflow_vector_delayed_lump): n<1!"
    else if (d < 0) then
       status = 2098 ! "ERROR(make_cashflow_vector_delayed_lump): d<0!"
    else if (d > n) then
       status = 2099 ! "ERROR(make_cashflow_vector_delayed_lump): d>n!"
    else
       cf_vec = 0
       cf_vec(1+d) = a
       status = 0
    end if
  end subroutine make_cashflow_vector_delayed_lump

  !------------------------------------------------------------------------------------------------------------------------------
  !> Create a cashflow of payments for a fixed annuity.
  !!
  !! The number of periods is assumed from the size of the cashflow vector at size(cashflow)-1.
  !! All payments other than the annuity payments are set to zero.
  !!
  !! @param cf_vec    The resulting cashflow vector.
  !! @param a         Annuity payment.
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2065-2096.
  !!
  subroutine make_cashflow_vector_delayed_level_annuity(cf_vec, a, d, e, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: a
    integer,       intent(in)  :: d, e
    integer,       intent(out) :: status
    ! Local Variables
    integer :: n
    ! Perform Computation
    n = size(cf_vec)-1
    if (n < 1) then
       status = 2065 ! "ERROR(make_cashflow_vector_delayed_level_annuity): n<1!"
    else if (d < 0) then
       status = 2066 ! "ERROR(make_cashflow_vector_delayed_level_annuity): d<0!"
    else if (d > n) then
       status = 2067 ! "ERROR(make_cashflow_vector_delayed_level_annuity): d>n!"
    else if (e < 0) then
       status = 2068 ! "ERROR(make_cashflow_vector_delayed_level_annuity): e<0!"
    else if (e > n) then
       status = 2069 ! "ERROR(make_cashflow_vector_delayed_level_annuity): e>n!"
    else if ((d+e) > n) then
       status = 2070 ! "ERROR(make_cashflow_vector_delayed_level_annuity): d+e>n!"
    else
       cf_vec = 0
       cf_vec(1+d:1+n-e) = a
       status = 0
    end if
  end subroutine make_cashflow_vector_delayed_level_annuity

  !------------------------------------------------------------------------------------------------------------------------------
  !> Create a cashflow of payments for a growing annuity.
  !!
  !! The number of periods is assumed from the size of the cashflow vector at size(cashflow)-1.
  !! All payments other than the annuity payments are set to zero.
  !!
  !! @param cf_vec    The resulting cashflow vector.
  !! @param g         Growth rate as a percentage.
  !! @param a         First annuity payment.
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2033-2064.
  !!
  subroutine make_cashflow_vector_delayed_geometric_annuity(cf_vec, g, a, d, e, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: g, a
    integer,       intent(in)  :: d, e
    integer,       intent(out) :: status
    ! Local Variables
    integer :: j, n
    ! Perform Computation
    n = size(cf_vec)-1
    if (n < 1) then
       status = 2033 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): n<1!"
    else if (d < 0) then
       status = 2034 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): d<0!"
    else if (d > n) then
       status = 2035 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): d>n!"
    else if (e < 0) then
       status = 2036 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): e<0!"
    else if (e > n) then
       status = 2037 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): e>n!"
    else if ((d+e) > n) then
       status = 2038 ! "ERROR(make_cashflow_vector_delayed_geometric_annuity): d+e>n!"
    else
       cf_vec = 0
       cf_vec(1+d:1+n-e) = [(a*(1+percentage_to_fraction(g))**j,j=0,n-e-d)]
       status = 0
    end if
  end subroutine make_cashflow_vector_delayed_geometric_annuity

  !------------------------------------------------------------------------------------------------------------------------------
  !> Create a cashflow of payments for an arithmatic annuity.
  !!
  !! The number of periods is assumed from the size of the cashflow vector at size(cashflow)-1.
  !! All payments other than the annuity payments are set to zero.
  !!
  !! @param cf_vec    The resulting cashflow vector.
  !! @param q         Amount added at each payment.
  !! @param a         First annuity payment.
  !! @param d         Delay from time zero.  i.e. d=0 is the beginning of period 1 otherwise d=j is the end if period j.
  !! @param e         Early end counted from time end (t=n). i.e. e=0 means the last payment is at end of period n.
  !! @param status    Returns status of computation. 0 if everything worked. Range: 0 & 2001-2032.
  !!
  subroutine make_cashflow_vector_delayed_arithmetic_annuity(cf_vec, q, a, d, e, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: q, a
    integer,       intent(in)  :: d, e
    integer,       intent(out) :: status
    ! Local Variables
    integer :: j, n
    ! Perform Computation
    n = size(cf_vec)-1
    if (n < 1) then
       status = 2001 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): n<1!"
    else if (d < 0) then
       status = 2002 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): d<0!"
    else if (d > n) then
       status = 2003 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): d>n!"
    else if (e < 0) then
       status = 2004 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): e<0!"
    else if (e > n) then
       status = 2005 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): e>n!"
    else if ((d+e) > n) then
       status = 2006 ! "ERROR(make_cashflow_vector_delayed_arithmetic_annuity): d+e>n!"
    else
       cf_vec = 0
       cf_vec(1+d:1+n-e) = [((a+q*j),j=0,n-e-d)]
       status = 0
    end if
  end subroutine make_cashflow_vector_delayed_arithmetic_annuity

  !------------------------------------------------------------------------------------------------------------------------------
  !> Add interest cashflows to a cashflow sequence as if the sequence were being added to an interest-bearing account over time.
  !!
  !! @param cf_vec  The cashflow vector to modify (one cashflow per period boundary).
  !! @param rate    The rate
  !! @param status  Returns status of computation. 0 if everything worked. Range: 0 & 2227-2259.
  !!
  subroutine add_intrest_to_cashflow_vector(cf_vec, rate, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: rate
    integer,       intent(out) :: status
    ! Local Variables
    integer       :: nb, j
    real(kind=rk) :: rsum
    ! Perform Computation
    nb = size(cf_vec)
    if (nb > 1) then
       rsum = cf_vec(1)
       do j=2,nb
          cf_vec(j) = cf_vec(j) + rsum * percentage_to_fraction(rate)
          rsum = rsum + cf_vec(j)
       end do
    end if
    status = 0
  end subroutine add_intrest_to_cashflow_vector

  !------------------------------------------------------------------------------------------------------------------------------
  !> Add interest cashflows to a cashflow sequence as if the sequence were being added to an interest baring account over time.
  !!
  !! @param cf_vec  The cashflow vector to modify (one cashflow per period boundary).
  !! @param vrate   A vector of rates (one rate per period).
  !! @param status  Returns status of computation. 0 if everything worked. Range: 0 & 2260-2292.
  !!
  subroutine add_multi_intrest_to_cashflow_vector(cf_vec, vrate, status)
    ! Arguments
    real(kind=rk), intent(out) :: cf_vec(:)
    real(kind=rk), intent(in)  :: vrate(:)
    integer,       intent(out) :: status
    ! Local Variables
    integer       :: nb, j
    real(kind=rk) :: rsum
    ! Perform Computation
    nb = size(cf_vec)
    if (nb > 1) then
       if (size(vrate) < (nb-1)) then
          status = 2260 ! "ERROR(add_multi_intrest_to_cashflow_vector): More periods than rates!"
       else
          rsum = cf_vec(1)
          do j=2,nb
             cf_vec(j) = cf_vec(j) + rsum * percentage_to_fraction(vrate(j-1))
             rsum = rsum + cf_vec(j)
          end do
       end if
    end if
    status = 0
  end subroutine add_multi_intrest_to_cashflow_vector

end module mrffl_cashflows

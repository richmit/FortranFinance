! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_life_table_print.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Test life_table_print subroutine.@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
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
program test_life_table_print
  use, intrinsic :: iso_fortran_env,   only: output_unit
  use, intrinsic :: iso_c_binding
  use            :: mrffl_config,      only: rk=>mrfflrk
  use            :: mrffl_life_table
  use            :: mrffl_prt_sets
  implicit none

  ! This is the qx data from the same data set we drew uscdc_qx_dat from in mrffl_life_table.
  real(kind=rk), parameter :: uscdc_qx_dat(0:100) = [ &
       0.005445689428598, 0.000402571138693, 0.000254236627370, 0.000191887069377, 0.000161350340932, 0.000142866410897, &
       0.000129908919916, 0.000119035452371, 0.000107113170088, 0.000095486117061, 0.000089930807007, 0.000099826858786, &
       0.000135970505653, 0.000204669006052, 0.000298894156003, 0.000405212253099, 0.000513052917086, 0.000622979365289, &
       0.000730827625375, 0.000836958352011, 0.000949320499785, 0.001064809854142, 0.001169851282611, 0.001258579082787, &
       0.001334920525551, 0.001405608723871, 0.001479519996792, 0.001560034928843, 0.001650915597565, 0.001748833688907, &
       0.001849254476838, 0.001946534845047, 0.002039701445028, 0.002128043444827, 0.002215563319623, 0.002308091381565, &
       0.002409243257716, 0.002518839202821, 0.002638272708282, 0.002768256934360, 0.002916490891948, 0.003078484907746, &
       0.003243524581194, 0.003409652970731, 0.003587225219235, 0.003792020725086, 0.004035838879645, 0.004315248690546, &
       0.004625073168427, 0.004959340672940, 0.005307620856911, 0.005685651674867, 0.006117654498667, 0.006619505118579, &
       0.007184223271906, 0.007766469381750, 0.008369085378945, 0.009041761048138, 0.009794997051358, 0.010606326162815, &
       0.011466973461211, 0.012333479709923, 0.013173462823033, 0.013980743475258, 0.014797933399677, 0.015665691345930, &
       0.016725668683648, 0.017852839082480, 0.019122175872326, 0.020525803789496, 0.021919380873442, 0.023536447435617, &
       0.025372466072440, 0.027616243809462, 0.029889062047005, 0.033725820481777, 0.036933455616236, 0.041016343981028, &
       0.044758260250092, 0.049530111253262, 0.054119721055031, 0.059482529759407, 0.065401390194893, 0.072224371135235, &
       0.080608516931534, 0.089139230549336, 0.099585749208927, 0.111021198332310, 0.123483829200268, 0.137000516057014, &
       0.151583865284920, 0.167229443788528, 0.183913290500641, 0.201589778065681, 0.220190301537514, 0.239622786641121, &
       0.259772449731827, 0.280503511428833, 0.301662474870682, 0.323082149028778, 1.000000000000000]

  ! This was captured from a previous run printing a life table using lx data.
  real(kind=rk), parameter :: uscdc_qxlx_dat(0:100) = [ &
       0.0054500, 0.0004022, 0.0002515, 0.0001912, 0.0001610, 0.0001409, 0.0001309, 0.0001208, 0.0001108, 0.0000906, &
       0.0000906, 0.0001007, 0.0001309, 0.0002116, 0.0002922, 0.0004132, 0.0005042, 0.0006255, 0.0007369, 0.0008284, &
       0.0009504, 0.0010727, 0.0011651, 0.0012577, 0.0013406, 0.0014034, 0.0014766, 0.0015604, 0.0016548, 0.0017496, &
       0.0018449, 0.0019510, 0.0020371, 0.0021238, 0.0022213, 0.0023090, 0.0024078, 0.0025176, 0.0026387, 0.0027607, &
       0.0029257, 0.0030710, 0.0032492, 0.0034080, 0.0035896, 0.0037943, 0.0040335, 0.0043076, 0.0046283, 0.0049642, &
       0.0053048, 0.0056836, 0.0061236, 0.0066156, 0.0071840, 0.0077640, 0.0083683, 0.0090442, 0.0097951, 0.0106019, &
       0.0114683, 0.0123391, 0.0131679, 0.0139784, 0.0148080, 0.0156590, 0.0167252, 0.0178537, 0.0191301, 0.0205141, &
       0.0219207, 0.0235375, 0.0253728, 0.0276152, 0.0298893, 0.0337251, 0.0369457, 0.0410071, 0.0447625, 0.0495257, &
       0.0541178, 0.0594839, 0.0653984, 0.0722311, 0.0806128, 0.0891265, 0.0995838, 0.1110298, 0.1234757, 0.1370290, &
       0.1515676, 0.1672204, 0.1839306, 0.2015437, 0.2201933, 0.2396694, 0.2597826, 0.2804699, 0.3017007, 0.3229420, &
       1.0000000]

  integer :: out_io_unit

  open(newunit=out_io_unit, file='test_life_table_print_usss_f_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_f_qx_dat, 100000)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_usss_f_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_f_lx_dat,      0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_usss_m_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_m_qx_dat, 100000)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_usss_m_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_m_lx_dat, 0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_wm_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_m_lx_dat, 0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_wf_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_f_lx_dat, 0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_w_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_lx_dat, 0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_lx_dat, 0)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_qx_dat, 100000)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='test_life_table_print_cdc_qxlx_dat.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_qxlx_dat, 100000)
  close(unit=out_io_unit, status='keep')

end program test_life_table_print

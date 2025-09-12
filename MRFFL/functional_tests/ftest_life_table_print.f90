! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      ftest_life_table_print.f90
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
program ftest_life_table_print
  use, intrinsic :: iso_fortran_env,   only: output_unit
!  use, intrinsic :: iso_c_binding
  use            :: mrffl_config,      only: rk=>mrfflrk, ik=>mrfflik
  use            :: mrffl_life_table,  only: life_table_print, usss_f_qx_dat, uscdc_lx_dat, uscdc_w_f_lx_dat, uscdc_w_lx_dat, uscdc_w_m_lx_dat, usss_f_lx_dat, usss_m_lx_dat, usss_m_qx_dat
  use            :: mrffl_prt_sets,    only: prt_title, prt_table, prt_ALL

  implicit none (type, external)

  ! This is the qx data from the same data set we drew uscdc_qx_dat from in mrffl_life_table.
  real(kind=rk), parameter :: uscdc_qx_dat(0:100) = [ &
       & 0.005445689428598_rk, 0.000402571138693_rk, 0.000254236627370_rk, 0.000191887069377_rk, 0.000161350340932_rk, &
       & 0.000142866410897_rk, 0.000129908919916_rk, 0.000119035452371_rk, 0.000107113170088_rk, 0.000095486117061_rk, &
       & 0.000089930807007_rk, 0.000099826858786_rk, 0.000135970505653_rk, 0.000204669006052_rk, 0.000298894156003_rk, &
       & 0.000405212253099_rk, 0.000513052917086_rk, 0.000622979365289_rk, 0.000730827625375_rk, 0.000836958352011_rk, &
       & 0.000949320499785_rk, 0.001064809854142_rk, 0.001169851282611_rk, 0.001258579082787_rk, 0.001334920525551_rk, &
       & 0.001405608723871_rk, 0.001479519996792_rk, 0.001560034928843_rk, 0.001650915597565_rk, 0.001748833688907_rk, &
       & 0.001849254476838_rk, 0.001946534845047_rk, 0.002039701445028_rk, 0.002128043444827_rk, 0.002215563319623_rk, &
       & 0.002308091381565_rk, 0.002409243257716_rk, 0.002518839202821_rk, 0.002638272708282_rk, 0.002768256934360_rk, &
       & 0.002916490891948_rk, 0.003078484907746_rk, 0.003243524581194_rk, 0.003409652970731_rk, 0.003587225219235_rk, &
       & 0.003792020725086_rk, 0.004035838879645_rk, 0.004315248690546_rk, 0.004625073168427_rk, 0.004959340672940_rk, &
       & 0.005307620856911_rk, 0.005685651674867_rk, 0.006117654498667_rk, 0.006619505118579_rk, 0.007184223271906_rk, &
       & 0.007766469381750_rk, 0.008369085378945_rk, 0.009041761048138_rk, 0.009794997051358_rk, 0.010606326162815_rk, &
       & 0.011466973461211_rk, 0.012333479709923_rk, 0.013173462823033_rk, 0.013980743475258_rk, 0.014797933399677_rk, &
       & 0.015665691345930_rk, 0.016725668683648_rk, 0.017852839082480_rk, 0.019122175872326_rk, 0.020525803789496_rk, &
       & 0.021919380873442_rk, 0.023536447435617_rk, 0.025372466072440_rk, 0.027616243809462_rk, 0.029889062047005_rk, &
       & 0.033725820481777_rk, 0.036933455616236_rk, 0.041016343981028_rk, 0.044758260250092_rk, 0.049530111253262_rk, &
       & 0.054119721055031_rk, 0.059482529759407_rk, 0.065401390194893_rk, 0.072224371135235_rk, 0.080608516931534_rk, &
       & 0.089139230549336_rk, 0.099585749208927_rk, 0.111021198332310_rk, 0.123483829200268_rk, 0.137000516057014_rk, &
       & 0.151583865284920_rk, 0.167229443788528_rk, 0.183913290500641_rk, 0.201589778065681_rk, 0.220190301537514_rk, &
       & 0.239622786641121_rk, 0.259772449731827_rk, 0.280503511428833_rk, 0.301662474870682_rk, 0.323082149028778_rk, &
       & 1.000000000000000_rk]


  ! This was captured from a previous run printing a life table using lx data.
  real(kind=rk), parameter :: uscdc_qxlx_dat(0:100) = [ &
       & 0.0054500_rk, 0.0004022_rk, 0.0002515_rk, 0.0001912_rk, 0.0001610_rk, 0.0001409_rk, 0.0001309_rk, 0.0001208_rk, &
       & 0.0001108_rk, 0.0000906_rk, 0.0000906_rk, 0.0001007_rk, 0.0001309_rk, 0.0002116_rk, 0.0002922_rk, 0.0004132_rk, &
       & 0.0005042_rk, 0.0006255_rk, 0.0007369_rk, 0.0008284_rk, 0.0009504_rk, 0.0010727_rk, 0.0011651_rk, 0.0012577_rk, &
       & 0.0013406_rk, 0.0014034_rk, 0.0014766_rk, 0.0015604_rk, 0.0016548_rk, 0.0017496_rk, 0.0018449_rk, 0.0019510_rk, &
       & 0.0020371_rk, 0.0021238_rk, 0.0022213_rk, 0.0023090_rk, 0.0024078_rk, 0.0025176_rk, 0.0026387_rk, 0.0027607_rk, &
       & 0.0029257_rk, 0.0030710_rk, 0.0032492_rk, 0.0034080_rk, 0.0035896_rk, 0.0037943_rk, 0.0040335_rk, 0.0043076_rk, &
       & 0.0046283_rk, 0.0049642_rk, 0.0053048_rk, 0.0056836_rk, 0.0061236_rk, 0.0066156_rk, 0.0071840_rk, 0.0077640_rk, &
       & 0.0083683_rk, 0.0090442_rk, 0.0097951_rk, 0.0106019_rk, 0.0114683_rk, 0.0123391_rk, 0.0131679_rk, 0.0139784_rk, &
       & 0.0148080_rk, 0.0156590_rk, 0.0167252_rk, 0.0178537_rk, 0.0191301_rk, 0.0205141_rk, 0.0219207_rk, 0.0235375_rk, &
       & 0.0253728_rk, 0.0276152_rk, 0.0298893_rk, 0.0337251_rk, 0.0369457_rk, 0.0410071_rk, 0.0447625_rk, 0.0495257_rk, &
       & 0.0541178_rk, 0.0594839_rk, 0.0653984_rk, 0.0722311_rk, 0.0806128_rk, 0.0891265_rk, 0.0995838_rk, 0.1110298_rk, &
       & 0.1234757_rk, 0.1370290_rk, 0.1515676_rk, 0.1672204_rk, 0.1839306_rk, 0.2015437_rk, 0.2201933_rk, 0.2396694_rk, &
       & 0.2597826_rk, 0.2804699_rk, 0.3017007_rk, 0.3229420_rk, 1.0000000_rk]

  integer :: out_io_unit
  open(newunit=out_io_unit, file='ftest_life_table_print_usss_f_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_f_qx_dat, 100000_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_usss_f_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_f_lx_dat,      0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_usss_m_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_m_qx_dat, 100000_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_usss_m_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_title+prt_table, usss_m_lx_dat, 0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_wm_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_m_lx_dat, 0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_wf_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_f_lx_dat, 0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_w_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_w_lx_dat, 0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_lx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_lx_dat, 0_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_qx.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_qx_dat, 100000_ik)
  close(unit=out_io_unit, status='keep')

  open(newunit=out_io_unit, file='ftest_life_table_print_cdc_qxlx_dat.txt', form='formatted', action='write')
  call life_table_print(out_io_unit, prt_ALL, uscdc_qxlx_dat, 100000_ik)
  close(unit=out_io_unit, status='keep')

end program ftest_life_table_print

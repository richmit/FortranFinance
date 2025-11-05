! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_prt_sets.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Constants to select what *_print subroutines will print.@EOL
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
!> Constants to select what *_print subroutines will print.
module mrffl_prt_sets
  implicit none (type, external)
  private

  integer, parameter, public :: prt_param       =          1   !< Print parameters before the table/titles
  integer, parameter, public :: prt_title       =          2   !< Print titles on the tables
  integer, parameter, public :: prt_table       =          4   !< Print a table
  integer, parameter, public :: prt_total       =          8   !< Print totals pv & fv after the table
  integer, parameter, public :: prt_space       =         16   !< Print vertical whitespace between parameters & table/titles & totals
  integer, parameter, public :: prt_fv          =         32   !< Print FV in table
  integer, parameter, public :: prt_fv_agg_val  =         64   !< Print FV aggregate in table
  integer, parameter, public :: prt_fv_agg_sum  =        128   !< Print FV aggregate running sum in table
  integer, parameter, public :: prt_pv          =        256   !< Print PV in table            
  integer, parameter, public :: prt_pv_agg_val  =        512   !< Print PV aggregate in table  
  integer, parameter, public :: prt_pv_agg_sum  =       1024   !< Print PV aggregate running sum in table      
  integer, parameter, public :: prt_cf          =       2048   !< Print CF in table            
  integer, parameter, public :: prt_cf_agg_val  =       4096   !< Print CF aggregate in table  
  integer, parameter, public :: prt_cf_agg_sum  =       8192   !< Print CF aggregate running sum in table      
  integer, parameter, public :: prt_U14         =      16384   !< Unallocated
  integer, parameter, public :: prt_U15         =      32768   !< Unallocated
  integer, parameter, public :: prt_U16         =      65536   !< Unallocated
  integer, parameter, public :: prt_U17         =     131072   !< Unallocated
  integer, parameter, public :: prt_U18         =     262144   !< Unallocated
  integer, parameter, public :: prt_U19         =     524288   !< Unallocated
  integer, parameter, public :: prt_U20         =    1048576   !< Unallocated
  integer, parameter, public :: prt_U21         =    2097152   !< Unallocated
  integer, parameter, public :: prt_U22         =    4194304   !< Unallocated
  integer, parameter, public :: prt_U23         =    8388608   !< Unallocated
  integer, parameter, public :: prt_U24         =   16777216   !< Unallocated
  integer, parameter, public :: prt_U25         =   33554432   !< Unallocated
  integer, parameter, public :: prt_U26         =   67108864   !< Unallocated
  integer, parameter, public :: prt_U27         =  134217728   !< Unallocated
  integer, parameter, public :: prt_U28         =  268435456   !< Unallocated
  integer, parameter, public :: prt_U29         =  536870912   !< Unallocated
  integer, parameter, public :: prt_U30         = 1073741824   !< Unallocated

  integer, parameter, public :: prt_NONE        =          0   !< Print nothing
  integer, parameter, public :: prt_ALL         = 2147483647   !< Print everything

  integer, parameter, public :: prt_all_agg_val  = prt_fv_agg_val + prt_pv_agg_val + prt_cf_agg_val
  integer, parameter, public :: prt_all_agg_sum  = prt_fv_agg_sum + prt_pv_agg_sum + prt_cf_agg_sum
  
end module mrffl_prt_sets

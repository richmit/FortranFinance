! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_bitset.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Simple sets (using the bits of an integer to indicate element existence).@EOL
!! @keywords  finance fortran monte carlo inflation cashflow time value of money tvm percentages taxes stock market
!! @std       F2023
!! @see       https://github.com/richmit/FortranFinance/MRFFL
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
!> Simple sets (using the bits of an integer to indicate element existence).
!!
!! This is *NOT* a compete implementation of bitsets.  Just what we need for MRFFL.  Today bitsets are used to identity
!! unknown variables for TVM solvers, and elements to print in output routines.
!!
module mrffl_bitset
  use mrffl_config, only: ik=>mrfflik
  implicit none  
  private

  public :: bitset_size, bitset_minus, bitset_subsetp, bitset_not_subsetp, bitset_intersectp, bitset_not_intersectp

contains

  !------------------------------------------------------------------------------------------------------------------------------
  !> Return the number of elements in bitset.
  !!
  integer pure function bitset_size(bitset)
    integer(kind=ik), intent(in) :: bitset
    bitset_size = popcnt(bitset)
  end function bitset_size

  !------------------------------------------------------------------------------------------------------------------------------
  !> Return the set diffrence between bitset1 and bitset2.
  !!
  integer pure function bitset_minus(bitset1, bitset2)
    integer(kind=ik), intent(in) :: bitset1, bitset2
    bitset_minus = iand(bitset1, not(bitset2))
  end function bitset_minus

  !------------------------------------------------------------------------------------------------------------------------------
  !!> Return .true. if bitset1 is a subset of bitset2, and .false. otherwise.
  !!
  logical pure function bitset_subsetp(bitset1, bitset2)
    integer(kind=ik), intent(in) :: bitset1, bitset2
    bitset_subsetp = (iand(bitset1, bitset2) == bitset1)
  end function bitset_subsetp

  !------------------------------------------------------------------------------------------------------------------------------
  !!> Return .true. if bitset1 is NOT a subset of bitset2, and .false. otherwise.
  !!
  logical pure function bitset_not_subsetp(bitset1, bitset2)
    integer(kind=ik), intent(in) :: bitset1, bitset2
    bitset_not_subsetp = (iand(bitset1, bitset2) /= bitset1)
  end function bitset_not_subsetp  

  !------------------------------------------------------------------------------------------------------------------------------
  !!> Return .true. if bitset1 and bitset2 have a non-empty intersection, and .false. otherwise.
  !!
  logical pure function bitset_intersectp(bitset1, bitset2)
    integer(kind=ik), intent(in) :: bitset1, bitset2
    bitset_intersectp = (iand(bitset1, bitset2) /= 0_ik)
  end function bitset_intersectp

  !------------------------------------------------------------------------------------------------------------------------------
  !!> Return .true. if bitset1 and bitset2 have an empty intersection, and .false. otherwise.
  !!
  logical pure function bitset_not_intersectp(bitset1, bitset2)
    integer(kind=ik), intent(in) :: bitset1, bitset2
    bitset_not_intersectp = (iand(bitset1, bitset2) == 0_ik)
  end function bitset_not_intersectp

end module mrffl_bitset

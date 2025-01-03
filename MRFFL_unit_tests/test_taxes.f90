! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      test_taxes.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_us_taxes.@EOL
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
program test_taxes
  use, intrinsic:: iso_c_binding
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  use mrffl_us_taxes
  use mrffl_tvm
  implicit none
  integer(kind=ik) i
  real(kind=rk)   r

  print *, "tax bracket: ", 0, max_bracket(0.0_rk-1.0_rk, tax_bracket_breaks_single)
  print *, "tax bracket: ", 0, max_bracket(0.0_rk+0.0_rk, tax_bracket_breaks_single)
  print *, "tax bracket: ", 0, max_bracket(0.0_rk+1.0_rk, tax_bracket_breaks_single)

  do i=1,size(tax_bracket_breaks_single)
     print *
     print *, "tax bracket: ", i, max_bracket(tax_bracket_breaks_single(i)-1.0_rk, tax_bracket_breaks_single)
     print *, "tax bracket: ", i, max_bracket(tax_bracket_breaks_single(i)+0.0_rk, tax_bracket_breaks_single)
     print *, "tax bracket: ", i, max_bracket(tax_bracket_breaks_single(i)+1.0_rk, tax_bracket_breaks_single)
  end do

  print *

  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 0, -1, effective_tax_rate(-1.0_rk, tax_bracket_breaks_single, tax_bracket_rates),   0.000000000000000     
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 0, +0, effective_tax_rate( 0.0_rk, tax_bracket_breaks_single, tax_bracket_rates),   0.000000000000000     
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 0, +1, effective_tax_rate( 1.0_rk, tax_bracket_breaks_single, tax_bracket_rates),  10.000000000000000     

  print *
  i = 1
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 1, -1, effective_tax_rate(tax_bracket_breaks_single(i)-1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), tax_bracket_rates(i)
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 1, +0, effective_tax_rate(tax_bracket_breaks_single(i)+0.0_rk, tax_bracket_breaks_single, tax_bracket_rates), tax_bracket_rates(i)
  r = 100*(tax_bracket_breaks_single(i)*tax_bracket_rates(i)/100+tax_bracket_rates(i+1)/100)/(tax_bracket_breaks_single(i)+1.0_rk)
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", 1, +1, effective_tax_rate(tax_bracket_breaks_single(i)+1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r

   do i=2,size(tax_bracket_breaks_single)-1
      print *
      r = 100 * (sum((tax_bracket_breaks_single(1:i)-[0.0_rk,tax_bracket_breaks_single(1:i-1)])*tax_bracket_rates(1:i)/100)-tax_bracket_rates(i)/100) / (tax_bracket_breaks_single(i)-1)
      print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, -1, effective_tax_rate(tax_bracket_breaks_single(i)-1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r
      r = 100 * (sum((tax_bracket_breaks_single(1:i)-[0.0_rk,tax_bracket_breaks_single(1:i-1)])*tax_bracket_rates(1:i)/100)) / (tax_bracket_breaks_single(i))
      print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, +0, effective_tax_rate(tax_bracket_breaks_single(i)+0.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r
      r = 100 * (sum((tax_bracket_breaks_single(1:i)-[0.0_rk,tax_bracket_breaks_single(1:i-1)])*tax_bracket_rates(1:i)/100)+tax_bracket_rates(i+1)/100) / (tax_bracket_breaks_single(i)+1.0_rk)
      print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, +1, effective_tax_rate(tax_bracket_breaks_single(i)+1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r
   end do

  print *

  i=size(tax_bracket_breaks_single)
  r = 100*sum(((tax_bracket_breaks_single(1:6) - [0.0_rk, tax_bracket_breaks_single(1:5)]) * tax_bracket_rates(1:6))/100) / (tax_bracket_breaks_single(i)-1)
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, -1, effective_tax_rate(tax_bracket_breaks_single(i)-1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r
  r = 100*(sum(((tax_bracket_breaks_single(1:6) - [0.0_rk, tax_bracket_breaks_single(1:5)]) * tax_bracket_rates(1:6))/100) + 1*tax_bracket_rates(i)/100)  / (tax_bracket_breaks_single(i)+0)
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, +0, effective_tax_rate(tax_bracket_breaks_single(i)+0.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r
  r = 100*(sum(((tax_bracket_breaks_single(1:6) - [0.0_rk, tax_bracket_breaks_single(1:5)]) * tax_bracket_rates(1:6))/100) + 2*tax_bracket_rates(i)/100)  / (tax_bracket_breaks_single(i)+1)
  print "(a10,i4,i5,f30.10,f30.10)", "tax rate: ", i, +1, effective_tax_rate(tax_bracket_breaks_single(i)+1.0_rk, tax_bracket_breaks_single, tax_bracket_rates), r

  print *

  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(    0.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 12.0_rk), 0.0000000000

  print *

  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(    1.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 0.000000000
  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(  100.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 0.000000000
  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax( 1000.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 0.000000000
  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(10000.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 0.000000000

  print *

  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(12759.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 12759.0_rk*10/100
  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(12760.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 12760.0_rk*10/100
  print "(a10,f30.10,f30.10)", "tax rate: ", projected_tax(12761.0_rk, tax_bracket_breaks_single, tax_bracket_rates, 2025, 10.0_rk), 12760.0_rk*10/100 + 12.0_rk/100


end program test_taxes

! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      utest_percentages.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-02
!! @brief     Unit Tests: mrffl_percentages.@EOL
!! @keywords  finance fortran monte carlo percentages cashflow time value of money tvm percentages taxes stock market
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
program utest_percentages
  use mrffl_config, only: rk=>mrfflrk !, ik=>mrfflik
  use mrffl_percentages

  implicit none

  print "(a20,*(f20.5))", " 5% of  150:",      percentage_of( 150.0_rk,  5.0_rk) ! 7.5000000000000000     
  print "(a20,*(f20.5))", " 5% of  100:",      percentage_of( 100.0_rk,  5.0_rk) ! 5.0000000000000000     
  print "(a20,*(f20.5))", " 5% of -150:",      percentage_of(-150.0_rk,  5.0_rk) ! -7.5000000000000000     
  print "(a20,*(f20.5))", " 5% of -100:",      percentage_of(-100.0_rk,  5.0_rk) ! -5.0000000000000000     
  print "(a20,*(f20.5))", "-5% of  150:",      percentage_of( 150.0_rk, -5.0_rk) ! -7.5000000000000000     
  print "(a20,*(f20.5))", "-5% of  100:",      percentage_of( 100.0_rk, -5.0_rk) ! -5.0000000000000000     
  print "(a20,*(f20.5))", "-5% of -150:",      percentage_of(-150.0_rk, -5.0_rk) ! 7.5000000000000000    
  print "(a20,*(f20.5))", "-5% of -100:",      percentage_of(-100.0_rk, -5.0_rk) ! 5.0000000000000000    
  print "(a20,*(f20.5))", " 5% of    0:",      percentage_of(   0.0_rk,  5.0_rk) ! 0.0000000000000000    
  print "(a20,*(f20.5))", " 0% of  150:",      percentage_of( 150.0_rk,  9.0_rk) ! 15.5000000000000000   

  print "(a20,*(f20.5))", " VEC:",             percentage_of([ 150.0_rk, 100.0_rk], [5.0_rk, 2.0_rk]);     ! 7.5000000000000000        2.0000000000000000     

  print "(a20,*(f20.5))", "100% to f:",        percentage_to_fraction(100.0_rk) ! 1.0000000000000000     
  print "(a20,*(f20.5))", " 50% to f:",        percentage_to_fraction( 50.0_rk) ! 0.50000000000000000     
  print "(a20,*(f20.5))", "  0% to f:",        percentage_to_fraction(  0.0_rk) ! 0.0000000000000000     
  print "(a20,*(f20.5))", "-50% to f:",        percentage_to_fraction(-50.0_rk) ! -0.50000000000000000     
                            
  print "(a20,*(f20.5))", "VEC:",              percentage_to_fraction([ 1.0_rk, 2.0_rk, 50.0_rk])  ! 1.0000000000000000E-002   2.0000000000000000E-002  0.50000000000000000     
                            
  print "(a20,*(f20.5))", "  1 to p:",         fraction_to_percentage( 1.0_rk) ! 100.00000000000000     
  print "(a20,*(f20.5))", " .5 to p:",         fraction_to_percentage( 0.5_rk) ! 50.000000000000000        
  print "(a20,*(f20.5))", "  0 to p:",         fraction_to_percentage( 0.0_rk) ! 0.0000000000000000       
  print "(a20,*(f20.5))", "-.5 to p:",         fraction_to_percentage(-0.5_rk) ! -50.000000000000000        
                            
  print "(a20,*(f20.5))", " VEC:",             fraction_to_percentage([ 1.0_rk, 2.0_rk, 0.5_rk]) ! 100.00000000000000        200.00000000000000        50.000000000000000     
                            
  print "(a20,*(f20.5))", " 150 to  100:",     percentage_change( 150.0_rk,  100.0_rk)   ! -33.333333333333329       
  print "(a20,*(f20.5))", " 100 to  150:",     percentage_change( 100.0_rk,  150.0_rk)   ! 50.000000000000000       
  print "(a20,*(f20.5))", "-150 to  120:",     percentage_change(-150.0_rk,  120.0_rk)   ! -180.00000000000000       
  print "(a20,*(f20.5))", "-120 to -150:",     percentage_change(-120.0_rk, -150.0_rk)   ! 25.000000000000000        
  print "(a20,*(f20.5))", "   0 to   50:",     percentage_change(   0.0_rk,   50.0_rk)   ! Infinity
                            
  print "(a20,*(f20.5))", " VEC:",             percentage_change( [150.0_rk, 100.0_rk], [ 100.0_rk, 150.0_rk])  ! -33.333333333333329        50.000000000000000     
                            
  print "(a20,*(f20.5))", " 150 to  100:",     percentage_of_total( 150.0_rk,  100.0_rk) ! 66.666666666666657  
  print "(a20,*(f20.5))", " 100 to  150:",     percentage_of_total( 100.0_rk,  150.0_rk) ! 150.00000000000000  
  print "(a20,*(f20.5))", "-150 to  120:",     percentage_of_total(-150.0_rk,  120.0_rk) ! -80.000000000000000 
  print "(a20,*(f20.5))", "-120 to -150:",     percentage_of_total(-120.0_rk, -150.0_rk) ! 125.00000000000000
  print "(a20,*(f20.5))", "   0 to   50:",     percentage_of_total(   0.0_rk,  50.0_rk)  ! Infinity
                            
  print "(a20,*(f20.5))", " VEC:",             percentage_of_total( [150.0_rk, 100.0_rk], [100.0_rk, 150.0_rk])  ! 66.666666666666657        150.00000000000000     

end program utest_percentages

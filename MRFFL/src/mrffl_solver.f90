! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_solver.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2024-12-19
!! @brief     Root solvers.@EOL
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

!----------------------------------------------------------------------------------------------------------------------------------
!> Root solvers.
!! 
module mrffl_solver
  use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
  implicit none  
  public

contains
  
  !------------------------------------------------------------------------------------------------------------------------------
  !> Search for a root for the function f in the interval [x0_init, x1_init].
  !! 
  !! The process is iterative.  At each step the size of the search interval is cut in half.  If the search interval gets too
  !! small (< x_epsilon), then the process is abandoned.  If no zero is found after max_itr, then the process is abandoned.
  !! 
  !! @param xc         The solution (or last value tested if no solution is found) 
  !! @param x0_init    Left side of search interval
  !! @param x1_init    Right side of search interval
  !! @param f          Function to solve for zero
  !! @param x_epsilon  Used to test if current search interval is too small
  !! @param y_epsilon  Used to test if f is near zero
  !! @param max_itr    Maximum number of iterations before giving up
  !! @param status     Returns status of computation. 0 if everything worked. Range: 0 & 4001-4032.
  !! @param progress   Print progress as solver searches for a solution
  !!
  subroutine bisection(xc, x0_init, x1_init, f, x_epsilon, y_epsilon, max_itr, status, progress)

    interface
       real(kind=rk) function func_to_solve_t(x)
         use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
         implicit none
         real(kind=rk), intent(in) :: x
       end function func_to_solve_t
    end interface

    procedure(func_to_solve_t)    :: f
    real(kind=rk),    intent(out) :: xc
    real(kind=rk),    intent(in)  :: x0_init, x1_init
    real(kind=rk),    intent(in)  :: x_epsilon, y_epsilon
    integer(kind=ik), intent(in)  :: max_itr
    integer(kind=ik), intent(out) :: status
    logical,          intent(in)  :: progress
    real(kind=rk)                 :: x0, x1, f0, f1, fc
    integer(kind=ik)              :: itr

    x0 = x0_init
    xc = x0
    f0 = f(xc)
    if (abs(f0) < y_epsilon) then
       status = 0   
    else
       x1 = x1_init
       xc = x1
       f1 = f(xc)
       if (abs(f1) < y_epsilon) then
          xc = x1
          status = 0   
       else
          if (progress)  print *, 0, x0, f0, x1, f1
          if (((f0 < 0) .and. (f1 < 0)) .or. ((f0 > 0) .and. (f1 > 0))) then
             status = 4001 ! "ERROR(bisection): f is the same sign on initial interval endpoints!"
          else
             do itr=1,max_itr
                xc = (x0 + x1) / 2
                if (abs(x0-x1) < x_epsilon) then
                   status = 4002 ! "ERROR(bisection): Interval is smaller than x_epsilon!"
                end if
                fc = f(xc)
                if (progress)  print *, itr, x0, f0, x1, f1, xc, fc
                if (abs(fc) < y_epsilon) then
                   status = 0
                   return
                end if
                if (fc < 0) then
                   if (f0 < 0) then ! && (f1 > 0)
                      x0 = xc
                      f0 = fc
                   else ! (f0 > 0) && (f1 < 0)
                      x1 = xc
                      f1 = fc
                   end if
                else ! (f>0)
                   if (f0 < 0) then ! && (f1 > 0)
                      x1 = xc
                      f1 = fc
                   else ! (f0 > 0) && (f1 < 0)
                      x0 = xc
                      f0 = fc
                   end if
                end if
             end do
             status = 4003 ! "ERROR(bisection): Failed to converge!"
          end if
       end if
    end if
  end subroutine bisection
  
  !------------------------------------------------------------------------------------------------------------------------------
  !> Use bisection() to search for a root for the function f in a list of intervals returning the first root found.
  !! 
  !! @param xc         The solution (or last value tested if no solution is found) 
  !! @param x0_init    A *VECTOR* of left sides for search intervals
  !! @param x1_init    A *VECTOR* of right sides for search intervals
  !! @param f          Function to solve for zero
  !! @param x_epsilon  Used to test if current search interval is too small
  !! @param y_epsilon  Used to test if f is near zero
  !! @param max_itr    Maximum number of iterations before giving up
  !! @param status     Returns status of computation. 0 if everything worked. Range: 0 & 4033-4064.
  !! @param progress   Print progress as solver searches for a solution
  !!
  subroutine multi_bisection(xc, x0_init, x1_init, f, x_epsilon, y_epsilon, max_itr, status, progress)
    implicit none

    interface
       real(kind=rk) function func_to_solve_t(x)
         use mrffl_config, only: rk=>mrfflrk, ik=>mrfflik
         implicit none
         real(kind=rk), intent(in) :: x
       end function func_to_solve_t
    end interface

    procedure(func_to_solve_t)    :: f
    real(kind=rk),    intent(out) :: xc
    real(kind=rk),    intent(in)  :: x0_init(:), x1_init(:)
    real(kind=rk),    intent(in)  :: x_epsilon, y_epsilon
    integer(kind=ik), intent(in)  :: max_itr
    integer(kind=ik), intent(out) :: status
    logical,          intent(in)  :: progress
    integer(kind=ik)              :: interval, num_intervals

    num_intervals = size(x0_init, kind=ik)
    do interval=1,num_intervals
       if (progress) print *, "Bisection on [", x0_init(interval), ", ", x1_init(interval), "]"
       call bisection(xc, x0_init(interval), x1_init(interval), f, x_epsilon, y_epsilon, max_itr, &
            status, progress)
       if (status == 0) then
          return
       end if
    end do
    status = 4033 ! "ERROR(bisection): No root found in any interval!"
  end subroutine multi_bisection

end module mrffl_solver

# -*- Mode:make; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      setup_fortran.mk
# @author    Mitch Richling http://www.mitchr.me/
# @brief     Set variables for fortran development.@EOL
# @keywords  
# @std       GNUmake BSDmake SYSVmake GenericMake
# @see       https://github.com/richmit/FortranFinance/
# @copyright 
#  @parblock
#  Copyright (c) 2025, Mitchell Jay Richling <http://www.mitchr.me/> All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#  
#  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
#  
#  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#  
#  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
#  DAMAGE.
#  @endparblock
# @filedetails
#
#  This include file is used to set variables for Fortran development:
#
#    - SETUP_FORTRAN_PATH .. Set to the absolute path of this make file
#    - EXE_SUFFIX .......... Set to .exe on windows/dos and the empty string otherwise
#    - SLB_SUFFIX .......... Set to .dll for windows/dos and .so otherwise
#    - OBJ_SUFFIX .......... Set to .obj for windows/dos and the empty string otherwise
#    - AR .................. Archive command
#    - FC .................. Fortran compiler command
#    - FFLAGS .............. Fortran flags
#    - FSHFLG .............. Flags to create a shared library
#  
#.  Several variables can be set before inclusion to tweak the results:
#
#    - Things which may be enabled (set to a non-empty string to enable):
#	   
#        - FCOMP_OPT_XSTACK ... Executable stack
#        - FCOMP_OPT_WARN ..... Warnings
#        - FCOMP_OPT_XWARN .... Extra warnings
#        - FCOMP_OPT_OMP ...... OpenMP 
#        - FCOMP_OPT_OPT ...... Fully optimize
#	   
#    - Options requiring a string:
#	   
#        - FCOMP .............. Compiler to use: gfortran (default if unset), flang, ifx, lfortran, nvfortran, NONE.
#        - FCOMP_STD .......... Set to language standard: 2023 (default for non-flang), 2018 (default for flang)
#
#  The variables SETUP_FORTRAN_PATH, EXE_SUFFIX, SLB_SUFFIX, & OBJ_SUFFIX are always set -- even when FCOMP=NONE.  The remaining variables will only be set
#  when FCOMP is set to a supported compiler name (i.e. not 'NONE').  If FCOMP is set to NONE, then it is expected that the including makefile will set these
#  variables as required.
#
#  Support status of compilers:
#
#            |--------------------------------------------------------------------------|
#            | Compiler  | MSYS2 | Linux | XSTACK | WARN | XWARN | OMP | OPT    | STD   |
#            |-----------+-------+-------+--------+------+-------+-----+--------+-------|
#            | gfortran  | YES   | YES   | YES(1) | YES  | YES   | YES | YES    | YES   |
#            | ifx       | YES   | YES   | YES    | YES  | YES   | YES | YES(4) | YES   |
#            | flang     | YES   | YES   | YES    | YES  | YES   | YES | YES    | YES(3)|
#            | nvfortran | YES   | YES   | YES    | YES  | NO	 | NO  | YES    | NO(2) |
#            | lfortran  | YES   | YES   | NO     | NO   | NO    | NO  | NO     | YES   |
#            |--------------------------------------------------------------------------|
#
#        Notes:
#
#            (1) gfortran automatically makes the stack executable and so the FCOMP_OPT_XSTACK is ignored.
#            (2) nvfortran has no command line argument for language standard.
#            (3) In addition to the language standard, flang has the -pedantic flag added.
#            (4) When optimization is enabled the floating point model is set to 'precise'.
#
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get the path for the other include files -- they are in the directory with this one.
SETUP_FORTRAN_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# By default MSYS2 on windows has an environment variable named "OS" set to "Windows_NT".
# I use this variable to adjust the names of object files, shared library files, and executable files.
ifeq ($(OS),Windows_NT)
	EXE_SUFFIX := .exe
	SLB_SUFFIX := .dll
	OBJ_SUFFIX := .obj
else
	EXE_SUFFIX :=
	SLB_SUFFIX := .so
	OBJ_SUFFIX := .o
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set FCOMP if it is not already set
ifndef FCOMP
  FCOMP=gfortran
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set FCOMP_STD if it is not already set
ifndef FCOMP_STD
  ifeq ($(FCOMP),flang) 
    FCOMP_STD=2018
  else
    FCOMP_STD=2023
  endif
endif

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Include compiler specific stuff
include $(SETUP_FORTRAN_PATH)/tools_$(FCOMP).mk

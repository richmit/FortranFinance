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
#  This include is used to setup variables for Fortran development.
#
#  The follwing variables are always set:
#  
#    - SETUP_FORTRAN_PATH .. Set to the absolute path of this make file
#    - EXE_SUFFIX .......... Set to .exe on windows/dos and the empty string otherwise
#    - SLB_SUFFIX .......... Set to .dll for windows/dos and .so otherwise
#    - OBJ_SUFFIX .......... Set to .obj for windows/dos and the empty string otherwise
#  
#  Valid values for FCOMP are: gfortran (default used if FCOMP is unset), flang, ifx, lfortran, nvfortran, NONE.
#  
#  When FCOMP is set to a compiler name (i.e. not 'NONE'), the compiler specific include will set the following variables:
#    - AR ....... Archive command
#    - FC ....... Fortran compiler command
#    - FFLAGS ... Fortran flags
#    - FSHFLG ... Flags to create a shared library
#   
#  If FCOMP is set to NONE, then it is expected that the includeing makefile will set these variables as required.
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
# Include compiler specific stuff
include $(join $(SETUP_FORTRAN_PATH), tools_$(FCOMP).mk)

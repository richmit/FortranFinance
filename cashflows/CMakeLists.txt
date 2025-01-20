# -*- Mode:cmake; Coding:us-ascii-unix; fill-column:158 -*-
#########################################################################################################################################################.H.S.##
##
# @file      CMakeLists.txt
# @author    Mitch Richling http://www.mitchr.me/
# @date      2025-01-20
# @version   VERSION
# @brief     Example CMakeLists.txt file showing how to find the exported configuration from MRFFL.@EOL
# @keywords  cmake make mrffl
# @std       cmake
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
#########################################################################################################################################################.H.E.##

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
cmake_minimum_required(VERSION 3.30)

project(MRFFL_cashflow_examples VERSION 0.1
        DESCRIPTION "Examples for MRFFL_cashflows module"
        LANGUAGES Fortran)

enable_language(Fortran)

set(CMAKE_Fortran_PREPROCESS NO)

# Find and include the MRFFL.cmake file.  This will setup the MRFFL library -- it's include dirs and link dir.
if(EXISTS "../../MRFFL/build/MRFFL.cmake")
  message(STATUS " MRFFL Search: Found: Exported")
  include("../MRFFL/build/MRFFL.cmake")
  if(NOT (EXISTS "../../MRFFL/build/modules/mrffl_config.mod") )
    message(ERROR " MRFFL Search: Found exported cmake file, but MRFFL library has not been built!!!")
  endif()
else()
  message(ERROR " MRFFL Search: Failed!!!")
endif()

add_executable(irr irr.f90)
target_link_libraries(irr PRIVATE MRFFL)

# Instead of loading the MRFFL.cmake file, you could directly set the include and link directories like this:
#    target_include_directories(irr AFTER PRIVATE ../MRFFL/build/modules)
#    target_link_directories(irr BEFORE PRIVATE ../MRFFL/build/lib)

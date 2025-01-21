cmake_minimum_required(VERSION 3.30)

project(MRFFL VERSION 0.1
        DESCRIPTION "MRFFL: xxx"
        LANGUAGES Fortran)

enable_language(Fortran)

set(CMAKE_Fortran_PREPROCESS NO)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)

if (UNIX)
  message(STATUS " MRFFL Compiler Setup: OS: UNIX")
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Flang$")
    set(CMAKE_Fortran_FLAGS, "-pedantic -std=f2018 -Wl,-z,execstack")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(CMAKE_Fortran_FLAGS, "-O3 -Wall -W -Xlinker -z -Xlinker execstack")
  elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    set(CMAKE_Fortran_FLAGS, "-stand f23 -Xlinker -z -Xlinker execstack")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS, "-O3 -Wall -Wsurprising -W -std=f2023")
  else()
    message(STATUS " MRFFL Compiler Setup: Compiler Unknown: ${CMAKE_Fortran_COMPILER_ID}")
  endif()
else()
  message(STATUS " MRFFL Compiler Setup: OS: NOT UNIX")
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Flang$")
    set(CMAKE_Fortran_FLAGS, "-pedantic -std=f2018 -Wl,-z,execstack")
    message(STATUS " MRFFL Compiler Setup: Flang on Windows is not well supported by MRFFL!")
  elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
    set(CMAKE_Fortran_FLAGS, "-stand:f23 -Qdiag-disable:5268")
  elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS, "-O3 -Wall -Wsurprising -W -std=f2023")
  else()
    message(STATUS " MRFFL Compiler Setup: Compiler Unknown: ${CMAKE_Fortran_COMPILER_ID}")
  endif()
endif (UNIX)

set(MY_MODS ../src/mrffl_config.f90
            ../src/mrffl_stats.f90
            ../src/mrffl_bitset.f90
            ../src/mrffl_prt_sets.f90
            ../src/mrffl_var_sets.f90
            ../src/mrffl_percentages.f90
            ../src/mrffl_us_inflation.f90
            ../src/mrffl_solver.f90
            ../src/mrffl_solver_ne.f90
            ../src/mrffl_tvm.f90
            ../src/mrffl_us_taxes.f90
            ../src/mrffl_cashflows.f90
            ../src/mrffl_tvm12.f90
            ../src/mrffl_us_markets.f90
            ../src/mrffl_life_table.f90
            CACHE INTERNAL "")

add_library(MRFFL "${MY_MODS}")

target_include_directories(MRFFL INTERFACE "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>")
target_link_directories(MRFFL INTERFACE BEFORE PRIVATE "$<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/lib>")
export(TARGETS MRFFL FILE "${CMAKE_BINARY_DIR}/MRFFL.cmake")

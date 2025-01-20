cmake_minimum_required(VERSION 3.30)

project(MRFFL VERSION 0.1
        DESCRIPTION "MRFFL: xxx"
        LANGUAGES Fortran)

enable_language(Fortran)

set(CMAKE_Fortran_PREPROCESS NO)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)

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

# rm -rf *; cmake .. -DCMAKE_Fortran_COMPILER=gfortran  -DCMAKE_Fortran_FLAGS="-O3 -Wall -Wsurprising -W -std=f2023"
# rm -rf *; cmake .. -DCMAKE_Fortran_COMPILER=ifx       -DCMAKE_Fortran_FLAGS="-stand f23 -Xlinker -z -Xlinker execstack"
# rm -rf *; cmake .. -DCMAKE_Fortran_COMPILER=nvfortran -DCMAKE_Fortran_FLAGS="-O3 -Wall -W -Xlinker -z -Xlinker execstack"
# rm -rf *; cmake .. -DCMAKE_Fortran_COMPILER=flang     -DCMAKE_Fortran_FLAGS="-pedantic -std=f2018 -Wl,-z,execstack"
# cmake --build .

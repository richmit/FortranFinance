! -*- Mode:F90; Coding:us-ascii-unix; fill-column:129 -*-
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!.H.S.!!
!>
!! @file      mrffl_life_table.f90
!! @author    Mitch Richling http://www.mitchr.me/
!! @date      2025-01-09
!! @brief     This module provides life table computations, and some US life table data.@EOL
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
!> Life table computations, and some US life table data.
!!
!! @par Life Table Data
!! The following tables are provided as public arrays exported from this module:
!!  - @f$ l_x @f$ for males as recorded in the 2021 period life table for the US Social Security Administration.
!!  - @f$ l_x @f$ for females as recorded in the 2021 period life table for the US Social Security Administration.
!!  - @f$ q_x @f$ for males as recorded in the 2021 period life table for the US Social Security Administration.
!!  - @f$ q_x @f$ for females as recorded in the 2021 period life table for the US Social Security Administration.
!!  - @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White Females
!!  - @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White Males
!!  - @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White
!!  - @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021
!!
!! @par Computation
!!
!! @par `probability_of_death`
!! @f$ q_x @f$ is the probability that someone aged exactly @f$ x @f$  will die before reaching age @f$ (x+1) @f$.
!! @f[ q_x = 1 - \frac{l_{x+1}}{l_x} @f]
!!
!! @warning 
!! It is common for the value of @f$ q_x @f$ to be adjusted.  It is also common for life tables with adjusted @f$ q_x @f$ values
!! to still report the actual values for @f$ l_x @f$ -- thus making the two columns inconsistent with each other with respect to
!! the above formula.  The 2021 Social Security life table is an example.  This module treats this single life table as two
!! separate, and independent entities -- this allows us to use whichever column is most appropriate for our application.
!!
!! @par `probability_of_survival_1`
!! @f$ p_x @f$ is the probability that someone aged exactly @f$ x @f$  will survive to age @f$ (x+1) @f$.
!! @f[ p_x =  1 - q_x            @f]
!! @f[ p_x = \frac{l_{x+1}}{l_x} @f]
!!
!! @par `cohort_size`
!! @f$ l_0 @f$ is the number of people in the cohort (sometimes called the "radix").  
!!
!! @par `survivors`
!! @f$ l_x @f$ is the number of people who survive to age @f$ x @f$.
!! @f[ l_x = (1 - q_x)l_{x-1} \text{ with } l_0 \text{ known}@f]
!!
!! @par `died`
!! @f$ d_x @f$ is the number of people who die aged @f$ x @f$  -- i.e. last birthday was age @f$ x @f$.
!! @f[ d_x = l_x q_x       @f]
!! @f[ d_x = l_x - l_{x+1} @f]
!!
!! @par `probability_of_survival_n`
!! @f$ _tp_x @f$ the probability that someone aged exactly @f$ x @f$ will survive for @f$ t @f$ more years living up to 
!! at least age @f$ x+t @f$.
!! @f[ _tp_x = \frac{l_{x+t}}{l_x} @f]
!!
!! @par `person_years`
!! @f$ L_x @f$ the total number of person-years lived by the cohort from age @f$ x @f$ to @f$ x+1 @f$.
!! @f[ L_x = l_{x+1} + \frac{d_x}{2} @f]
!!
!! @warning 
!! I like the simple approach of assuming people die uniformly on the time interval.  That is to say the average number of years
!! lived by people who died on the interval was 0.5 years.  There are several other methodologies in common use today that do not
!! make this assumption.  The most common approaches are to use some kind of smoothing criteria.  For a popular example, see
!! "Keyfitz N. A life table that agrees with the data. Journal of the American Statistical Association. 1966 Jun 1".
!!
!! @par `total_person_years`
!! @f$ T_x @f$ the total number of person-years lived by the cohort from age @f$ x @f$ until all have died.
!! @f[ T_x = \sum_{k=x}^\infty L_k @f]
!!
!! @par `life_expectancy`
!! @f$ e_x @f$ is the average number of years of life remaining at age @f$ x @f$.
!! This value is provided directly in our data table, so we do *not* use the approximation below.
!! @f[ e_x = \frac{T_x}{l_x} @f]
!!
!! @par `mortality_rate`
!! @f$ m_x @f$ is the mortality rate at age @f$ x @f$. 
!! @f[ m_x = \frac{d_x}{L_x} @f]
!!
!! @par life_table & cohort parameters
!! Every function in this library takes `life_table` and `cohort_size` as the last two arguments.  These arguments are used in
!! the same way in all functions, and are documented here:
!!  - `life_table` is a rank 1 array containing life table data enumerated at whole number ages starting at 0 and incrementing by
!!     one for each element.  It may contain values for the survival function (number of people alive at the given age) or the
!!     probability_of_death function (the probability of someone at a given age expiring within one year).  The upper and lower
!!     bounds are irrelevant; however, it is very important that the first element of the array express the value for the
!!     function at age 0. I normally use a lower bound of zero.
!!  - `cohort_size` is used to identify the type of functional data stored in `life_table`, and to specify the cohort size when
!!    that functional data is the probability_of_death.  When `cohort_size <= 0` the `life_table` is assumed to contain the survival
!!    function (in this case the cohort size is the first element of the `life_table` array).  When `cohort_size > 0`, the
!!    `life_table` is assumed to contain the probability of death function and the value of `cohort_size` is used to compute the
!!    survival function at age 0 - i.e. it is used as the cohort size.
!!
module mrffl_life_table
  use mrffl_config,   only: rk=>mrfflrk, ik=>mrfflik, zero_epsilon
  use mrffl_prt_sets, only: prt_title, prt_table, prt_space
  use mrffl_bitset,   only: bitset_subsetp
  implicit none

  private

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ for males as recorded in the 2021 period life table for the US Social Security Administration.
  !!
  real(kind=rk), public, parameter :: usss_m_lx_dat(0:119) = [ &
       100000, 99414, 99372, 99345, 99323, 99304, 99289, 99275, 99262, 99250, &
        99238, 99225, 99211, 99195, 99173, 99143, 99098, 99035, 98949, 98840, &
        98715, 98579, 98433, 98275, 98106, 97926, 97734, 97530, 97316, 97089, &
        96850, 96601, 96342, 96073, 95797, 95512, 95218, 94916, 94603, 94277, &
        93937, 93582, 93211, 92825, 92423, 92003, 91564, 91100, 90608, 90083, &
        89523, 88926, 88289, 87606, 86874, 86089, 85248, 84347, 83386, 82361, &
        81272, 80112, 78882, 77582, 76215, 74786, 73296, 71749, 70141, 68468, &
        66732, 64927, 63046, 61080, 59018, 56849, 54543, 52126, 49598, 46958, &
        44198, 41342, 38409, 35420, 32385, 29314, 26234, 23175, 20178, 17298, &
        14571, 12029,  9707,  7640,  5863,  4386,  3198,  2271,  1572,  1060, &
          698,   448,   279,   169,    99,    56,    30,    16,     8,     4, &
            2,     1,     0,     0,     0,     0,     0,     0,     0,     0]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ for females as recorded in the 2021 period life table for the US Social Security Administration.
  !!
  real(kind=rk), public, parameter :: usss_f_lx_dat(0:119) = [ &
       100000, 99494, 99455, 99432, 99415, 99400, 99388, 99378, 99367, 99358, &
        99348, 99338, 99327, 99314, 99298, 99279, 99256, 99227, 99192, 99150, &
        99105, 99055, 98999, 98939, 98873, 98802, 98725, 98643, 98555, 98462, &
        98361, 98252, 98135, 98008, 97873, 97730, 97579, 97420, 97252, 97075, &
        96887, 96687, 96474, 96247, 96008, 95756, 95489, 95203, 94897, 94568, &
        94215, 93837, 93433, 93000, 92537, 92040, 91503, 90924, 90303, 89635, &
        88915, 88142, 87313, 86427, 85490, 84502, 83470, 82389, 81248, 80041, &
        78758, 77393, 75934, 74369, 72686, 70872, 68895, 66764, 64485, 62059, &
        59469, 56714, 53803, 50741, 47530, 44170, 40672, 37065, 33386, 29698, &
        26043, 22471, 19042, 15814, 12847, 10192,  7890,  5956,  4385,  3149, &
         2208,  1509,  1003,   646,   402,   241,   139,    77,    40,    20, &
            9,     4,     2,     1,     0,     0,     0,     0,     0,     0]
  
  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ q_x @f$ for males as recorded in the 2021 period life table for the US Social Security Administration.
  !! cohort_size=100000
  !! 
  real(kind=rk), public, parameter :: usss_m_qx_dat(0:119) = [ &
       0.0058600, 0.0004200, 0.0002720, 0.0002250, 0.0001840, 0.0001570, 0.0001400, 0.0001280, 0.0001220, 0.0001230, &
       0.0001290, 0.0001380, 0.0001640, 0.0002200, 0.0003100, 0.0004460, 0.0006370, 0.0008680, 0.0011000, 0.0012700, &
       0.0013730, 0.0014880, 0.0016050, 0.0017140, 0.0018350, 0.0019630, 0.0020820, 0.0022020, 0.0023300, 0.0024570, &
       0.0025740, 0.0026830, 0.0027870, 0.0028810, 0.0029740, 0.0030740, 0.0031750, 0.0032950, 0.0034440, 0.0036080, &
       0.0037800, 0.0039580, 0.0041440, 0.0043370, 0.0045400, 0.0047740, 0.0050640, 0.0053990, 0.0057960, 0.0062140, &
       0.0066710, 0.0071670, 0.0077360, 0.0083510, 0.0090350, 0.0097700, 0.0105670, 0.0113980, 0.0122910, 0.0132240, &
       0.0142670, 0.0153530, 0.0164840, 0.0176170, 0.0187590, 0.0199140, 0.0211040, 0.0224230, 0.0238470, 0.0253570, &
       0.0270500, 0.0289700, 0.0311880, 0.0337540, 0.0367470, 0.0405630, 0.0443080, 0.0484980, 0.0532290, 0.0587780, &
       0.0646170, 0.0709470, 0.0778340, 0.0856860, 0.0948090, 0.1050900, 0.1165920, 0.1293060, 0.1427320, 0.1576380, &
       0.1744580, 0.1930270, 0.2129300, 0.2326570, 0.2518260, 0.2709430, 0.2897560, 0.3079980, 0.3253930, 0.3416620, &
       0.3587460, 0.3766830, 0.3955170, 0.4152930, 0.4360580, 0.4578600, 0.4807530, 0.5047910, 0.5300310, 0.5565320, &
       0.5843590, 0.6135770, 0.6442560, 0.6764680, 0.7102920, 0.7458060, 0.7830970, 0.8222510, 0.8633640, 0.9065320]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ q_x @f$ for females as recorded in the 2021 period life table for the US Social Security Administration.
  !! cohort_size=100000
  !!
  real(kind=rk), public, parameter :: usss_f_qx_dat(0:119) = [ &
       0.0050630, 0.0003930, 0.0002230, 0.0001770, 0.0001440, 0.0001220, 0.0001090, 0.0001020, 0.0000980, 0.0000970, &
       0.0001030, 0.0001130, 0.0001310, 0.0001570, 0.0001900, 0.0002330, 0.0002910, 0.0003550, 0.0004180, 0.0004610, &
       0.0005070, 0.0005560, 0.0006100, 0.0006660, 0.0007220, 0.0007750, 0.0008310, 0.0008890, 0.0009520, 0.0010250, &
       0.0011040, 0.0011920, 0.0012890, 0.0013830, 0.0014650, 0.0015440, 0.0016260, 0.0017190, 0.0018240, 0.0019400, &
       0.0020660, 0.0022020, 0.0023510, 0.0024820, 0.0026220, 0.0027890, 0.0029940, 0.0032190, 0.0034670, 0.0037290, &
       0.0040110, 0.0043060, 0.0046340, 0.0049810, 0.0053700, 0.0058310, 0.0063260, 0.0068370, 0.0073990, 0.0080330, &
       0.0086870, 0.0094110, 0.0101390, 0.0108490, 0.0115500, 0.0122160, 0.0129520, 0.0138440, 0.0148630, 0.0160280, &
       0.0173290, 0.0188590, 0.0206090, 0.0226200, 0.0249580, 0.0279060, 0.0309250, 0.0341400, 0.0376200, 0.0417250, &
       0.0463240, 0.0513340, 0.0569110, 0.0632790, 0.0707040, 0.0791840, 0.0886970, 0.0992400, 0.1104800, 0.1230780, &
       0.1371520, 0.1526050, 0.1694940, 0.1876230, 0.2066470, 0.2258900, 0.2450540, 0.2638150, 0.2818280, 0.2987380, &
       0.3166620, 0.3356620, 0.3558020, 0.3771500, 0.3997790, 0.4237660, 0.4491920, 0.4761430, 0.5047120, 0.5349940, &
       0.5670940, 0.6011200, 0.6371870, 0.6754180, 0.7102920, 0.7458060, 0.7830970, 0.8222510, 0.8633640, 0.9065320]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White Females
  !!
  real(kind=rk), public, parameter :: uscdc_w_f_lx_dat(0:100) = [ &
       100000, 99594, 99565, 99547, 99536, 99526, 99517, 99509, 99501, 99494, &
        99487, 99480, 99472, 99463, 99450, 99433, 99410, 99383, 99351, 99314, &
        99273, 99227, 99176, 99120, 99059, 98992, 98920, 98843, 98759, 98667, &
        98566, 98457, 98339, 98212, 98077, 97933, 97781, 97619, 97447, 97265, &
        97074, 96871, 96655, 96426, 96185, 95929, 95658, 95368, 95057, 94725, &
        94371, 93996, 93596, 93169, 92709, 92213, 91680, 91110, 90497, 89837, &
        89127, 88364, 87548, 86684, 85779, 84833, 83844, 82800, 81693, 80509, &
        79235, 77876, 76421, 74863, 73179, 71381, 69377, 67214, 64874, 62393, &
        59748, 56950, 54018, 50925, 47681, 44270, 40716, 37095, 33384, 29635, &
        25909, 22273, 18795, 15540, 12566,  9919,  7628,  5706,  4143,  2915, & 
         1985]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White Males
  !!
  real(kind=rk), public, parameter :: uscdc_w_m_lx_dat(0:100) = [ &
       100000, 99532, 99493, 99466, 99443, 99426, 99411, 99397, 99384, 99373, &
        99363, 99353, 99342, 99327, 99304, 99270, 99225, 99168, 99099, 99016, &
        98921, 98812, 98689, 98552, 98402, 98239, 98066, 97880, 97682, 97470, &
        97246, 97008, 96757, 96495, 96223, 95942, 95652, 95351, 95041, 94719, &
        94385, 94038, 93675, 93296, 92902, 92492, 92062, 91608, 91124, 90609, &
        90060, 89478, 88860, 88199, 87490, 86725, 85903, 85026, 84088, 83084, &
        82013, 80874, 79670, 78407, 77091, 75722, 74301, 72810, 71249, 69611, &
        67896, 66113, 64254, 62304, 60243, 58093, 55744, 53282, 50670, 47963, &
        45115, 42200, 39199, 36136, 33021, 29850, 26717, 23576, 20479, 17483, &
        14643, 12011,  9630,  7534,  5740,  4252,  3056,  2129,  1434,   934, &
          587]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021, White
  !!
  real(kind=rk), public, parameter :: uscdc_w_lx_dat(0:100) = [ &
       100000, 99562, 99527, 99505, 99488, 99475, 99463, 99451, 99441, 99432, &
        99423, 99415, 99406, 99393, 99375, 99349, 99315, 99273, 99221, 99161, &
        99092, 99014, 98926, 98828, 98721, 98606, 98482, 98349, 98206, 98053, &
        97889, 97714, 97528, 97333, 97128, 96914, 96691, 96458, 96215, 95962, &
        95697, 95420, 95128, 94822, 94502, 94167, 93814, 93439, 93039, 92613, &
        92158, 91675, 91162, 90613, 90024, 89388, 88706, 87978, 87197, 86360, &
        85464, 84506, 83490, 82419, 81301, 80137, 78925, 77650, 76310, 74893, &
        73392, 71814, 70151, 68390, 66512, 64531, 62348, 60029, 57547, 54948, &
        52197, 49338, 46371, 43294, 40117, 36832, 33508, 30123, 26725, 23367, &
        20105, 16996, 14093, 11443,  9082,  7035,  5308,  3895,  2775,  1918, &
         1283]

  !--------------------------------------------------------------------------------------------------------------------------------
  !> @f$ l_x @f$ from National Vital Statistics Reports, Volume 72, Number 12, United States Life Tables, 2021
  !!
  real(kind=rk), public, parameter :: uscdc_lx_dat(0:100) = [ &
       100000, 99455, 99415, 99390, 99371, 99355, 99341, 99328, 99316, 99305, &
        99296, 99287, 99277, 99264, 99243, 99214, 99173, 99123, 99061, 98988, &
        98906, 98812, 98706, 98591, 98467, 98335, 98197, 98052, 97899, 97737, &
        97566, 97386, 97196, 96998, 96792, 96577, 96354, 96122, 95880, 95627, &
        95363, 95084, 94792, 94484, 94162, 93824, 93468, 93091, 92690, 92261, &
        91803, 91316, 90797, 90241, 89644, 89000, 88309, 87570, 86778, 85928, &
        85017, 84042, 83005, 81912, 80767, 79571, 78325, 77015, 75640, 74193, &
        72671, 71078, 69405, 67644, 65776, 63810, 61658, 59380, 56945, 54396, &
        51702, 48904, 45995, 42987, 39882, 36667, 33399, 30073, 26734, 23433, &
        20222, 17157, 14288, 11660,  9310,  7260,  5520,  4086,  2940,  2053, & 
         1390]

  public :: life_table_print
  ! The functions directly use the life_table or cohort_size arguments to produce values.
  public :: survivors, probability_of_death
  ! These functions all call survivors and/or probability_of_death to derive the return value.  They do not access the life_table
  ! or cohort_size arguments directly.
  public :: died, life_expectancy, mortality_rate, person_years, probability_of_survival_1, probability_of_survival_n, total_person_years

contains

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ l_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk)  function survivors(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    integer(kind=ik)             :: i
    if (age > size(life_table)-1) then
       survivors = 0
    else
       if (cohort_size <= 0) then ! life_table contains lx
          if (age <= 0) then
             survivors = life_table(1)
          else
             survivors = life_table(age+1)
          end if
       else
          if (age <= 0) then
             survivors = cohort_size          
          else
             survivors = cohort_size
             do i=2,min(age+1, size(life_table))
                survivors = (1 - life_table(i-1)) * survivors
             end do
          end if
       end if
    end if
  end function survivors

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ q_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function probability_of_death(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    real(kind=rk)                :: tmp1
    if (age < 0) then
       probability_of_death = 0
    else if (age > size(life_table)-1) then
       probability_of_death = 1
    else
       if (cohort_size <= 0) then ! life_table contains lx
          if (age == size(life_table)-1) then
             probability_of_death = 1
          else
             tmp1 = life_table(age+1)
             if (abs(tmp1) < zero_epsilon) then
                probability_of_death = 1
             else
                probability_of_death = 1 - life_table(age+2) / tmp1
             end if
          end if
       else
          probability_of_death = life_table(age+1)
       end if
    end if
  end function probability_of_death

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ e_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function life_expectancy(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    real(kind=rk)                :: tmp1
    tmp1 = survivors(age, life_table, cohort_size)
    if (abs(tmp1) < zero_epsilon) then
       life_expectancy = 0
    else
       life_expectancy = total_person_years(age, life_table, cohort_size) / tmp1
    end if
  end function life_expectancy

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ _tp_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param n   Number of years.  Must be non-negative.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function probability_of_survival_n(age, n, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, n, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    real(kind=rk)                :: tmp1
    tmp1 = survivors(age, life_table, cohort_size)
    if (abs(tmp1) < zero_epsilon) then
       probability_of_survival_n = 0
    else
       probability_of_survival_n = survivors(age+n, life_table, cohort_size) / tmp1
    end if
  end function probability_of_survival_n

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ p_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function probability_of_survival_1(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    probability_of_survival_1 = probability_of_survival_n(age, 1, life_table, cohort_size)
    !Should also be equal to: 1- probability_of_death(age, life_table, cohort_size)
  end function probability_of_survival_1

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ d_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function died(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    died = survivors(age, life_table, cohort_size) - survivors(age+1, life_table, cohort_size)
  end function died

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ L_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function person_years(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    person_years = real(survivors(age+1, life_table, cohort_size), rk) + real(died(age, life_table, cohort_size), rk) / 2
  end function person_years

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ T_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function total_person_years(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    integer(kind=ik)             :: i
    total_person_years = 0
    do i=age,size(life_table)-1
       total_person_years = total_person_years + person_years(i, life_table, cohort_size)
    end do
  end function total_person_years

  !--------------------------------------------------------------------------------------------------------------------------------
  !> Return @f$ m_x @f$ for the given age.  See the module documentation for a guide to symbols.
  !!
  !! @param age Age of the person.
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  real(kind=rk) function mortality_rate(age, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in) :: age, cohort_size
    real(kind=rk), intent(in)    :: life_table(:)
    real(kind=rk)                :: tmp1
    tmp1 = person_years(age, life_table, cohort_size)
    if (abs(tmp1) < zero_epsilon) then
       mortality_rate = 1
    else
       mortality_rate = died(age, life_table, cohort_size) / tmp1
    end if
  end function mortality_rate
  
  !--------------------------------------------------------------------------------------------------------------------------------
  !> Print life table.
  !!
  !! An ERROR STOP occures if a WRITE causes an I/O error
  !!
  !! @param out_io_unit Unit to which to print table
  !! @param print_out   Bitset built from the following constants prt_title, prt_table, & prt_space
  !! @param life_table  Data for life table.  See module documentation for description.
  !! @param cohort_size Number of people in the cohort.  See module documentation for description.
  !!
  subroutine life_table_print(out_io_unit, print_out, life_table, cohort_size)
    implicit none
    integer(kind=ik), intent(in)  :: cohort_size, print_out
    real(kind=rk), intent(in)     :: life_table(:)
    integer, intent(in)           :: out_io_unit
    integer(kind=ik)              :: age
    character(len=:), allocatable :: fmt_t, fmt_n
    integer                       :: out_io_stat
    fmt_t = '(a5,a11,  a11,  a10,  a11,  a12,  a8,  a11,  a11  )'
    fmt_n = '(i5,f11.7,f11.2,f10.2,f11.2,f12.2,f8.2,f11.7,f11.7)'
    if (bitset_subsetp(prt_space, print_out)) then
       write (unit=out_io_unit, iostat=out_io_stat, fmt='(a)') ""
       if (out_io_stat /= 0) error stop "ERROR(life_table_print): I/O Error!"
    end if
    if (bitset_subsetp(prt_title, print_out)) then
       write (unit=out_io_unit, iostat=out_io_stat, fmt=fmt_t) &
            "age", "qx", "lx", "dx", "Lx", "Tx", "ex", "mx", "px"
       if (out_io_stat /= 0) error stop "ERROR(life_table_print): I/O Error!"
    end if
    if (bitset_subsetp(prt_table, print_out)) then
       do age=0,size(life_table)-1
          write (unit=out_io_unit, iostat=out_io_stat, fmt=fmt_n) &
               age, &
               probability_of_death(age, life_table, cohort_size), &
               survivors(age, life_table, cohort_size), &
               died(age, life_table, cohort_size), &
               person_years(age, life_table, cohort_size), &
               total_person_years(age, life_table, cohort_size), &
               life_expectancy(age, life_table, cohort_size), &
               mortality_rate(age, life_table, cohort_size), &
               probability_of_survival_1(age, life_table, cohort_size)
          if (out_io_stat /= 0) error stop "ERROR(life_table_print): I/O Error!"
       end do
    end if
    if (bitset_subsetp(prt_space, print_out)) then
       write (unit=out_io_unit, iostat=out_io_stat, fmt='(a)') ""
       if (out_io_stat /= 0) error stop "ERROR(life_table_print): I/O Error!"
    end if
  end subroutine life_table_print

end module mrffl_life_table

# Fortran Finance

Theoretical Finance Computation Using Fortran

Parts of this code have been with me for quite some time.  I
originally started this in college to help with a mathematical
modeling course.  Over the years it has collected more functionality
as my needs have expanded -- mostly investment applications.  Most
recently I've added quite a bit of stuff around retirement planning.

Much of the functionality is contained in a set of modules I call [MR Fortran Finance Library](https://richmit.github.io/FortranFinance/MRFFL/index.html).

You will find a few directories at the root of the repository covering
various computational finance topics:

  - [Retirement Simulator](https://richmit.github.io/FortranFinance/retirement_simulation/index.html) : Retirement Simulator Tool
  - [monte_carlo](https://richmit.github.io/FortranFinance/monte_carlo/index.html) : Illustrates basic resampling monte carlo for inflation and stock market returns.
  - [loans](https://richmit.github.io/FortranFinance/loans/index.html) : Various loan TVM problems.
  - retirement : Some TVM-style retirement problems.
  - MRFFL_functional_tests : Functional tests for MRFFL.  While these are basic MRFFL tests, they also demonstrate the API.
  - MRFFL_unit_tests : Unit tests for MRFFL.  
  - **More are on the way. I'm working to add the rest as time permits.**

### WARNING

Lastly, a warning.  I'm not a finance expert -- just an amateur
investor trying to plan my retirement.  So don't take anything you see
here as expert advice -- or even assume it's correct.  In fact, expect
errors!  Double check any results you get.  Keep an eye out for bugs,
and report what you find!

-mitch

PS: Have Fun!

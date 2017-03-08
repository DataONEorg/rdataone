## Test environments

* macOS 10.12.3 R 3.3.2, R 3.3.3
* Ubuntu 14.04.5, R 3.3.2
* Windows 7, R 3.3.2 (i386, x86_64)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.3.3 (2017-03-06)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R (unstable) (2017-03-07 r72317)

## Changes since last release

* Fixed a problem where the unit tests were failing due to an incompatibility with 
  testthat 1.0.2. All unit tests are now passing with testthat 1.0.2. (#171)
  
* uploadDataPackage() now uses the @cn slot to set the value for
  the default resolveURI (#170)
  
* All methods that send a PID to DataONE now property URLencode
  the PID. (#163)

## R CMD check results

* There were no ERRORs or WARNINGs.
* There was 1 NOTE:
  - A NOTE checking CRAN incoming feasibility:
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - API (4:40)
      - DataONE (4:27, 18:9, 18:53)
      - metadata (17:57, 21:34, 22:5)
* This release fixes testing errors due to incompatibility with a new release of the
  testing software testthat v 1.0.2. These issues have been resolved, and all tests pass
  now, and example code runs correctly.

## Downstream dependencies

* No downstream dependencies currently exist, as reported by devtools::revdep_check()

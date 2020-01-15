## Test environments

* macOS 10.l4.5 R 3.6.1
* Ubuntu 18.04 R 3.5
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.3 (2019-03-11)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2020-01-07 r77633)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.6.2 (2019-12-12)

## Changes since last release

* Ensure that DataONE service outages don't cause package tests to fail during CRAN testing.

## R CMD check results

* There was one note:
  Possibly mis-spelled words in DESCRIPTION:
     DataONE (4:27, 42:9, 43:10)
  - this is the correct spelling for the name of our organization. 
* There were no ERRORs, or WARNINGs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
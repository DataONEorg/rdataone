## Test environments

* macOS 10.l4.5 R 3.6.2
* Ubuntu 18.04 R 3.5
* Debian 9.4 R Under development (unstable) (2020-02-04 r7777
* Windows 10 R Under development
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.3
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.6.2 

## Changes since last release

* Fix CRAN check warnings for dataone 2.1.3 (#241)
* Mark deprecated function as defunct (#240)
* Fix roxygen2 errors related to function arguments (#241)
* remove knitr/RDS dependency on R 3.5
* Fix getDataPackage() failing with "subscript out of bounds" (#243)


## R CMD check results

* There was one note:
  Possibly mis-spelled words in DESCRIPTION:
     DataONE (4:27, 42:9, 43:10)
  - this is the correct spelling for the name of our organization. 
* There were no ERRORs, or WARNINGs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
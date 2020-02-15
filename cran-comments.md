## Test environments

 * macOS 10.14: R 3.6.2
 * Ubuntu 18.10 R 3.6.2
 * Debian 9.4: R Under development (unstable) (2020-02-04 r77771)
 * Windows 10: R Under development
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R 3.6.2
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) Under development (unstable)
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.3
 * rhub::check_for_cran()
   * Fedora Linux, R-devel, clang, gfortran
   * Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental)
   * Fedora Linux, R-devel, GCC
   * Fedora Linux, R-devel, clang, gfortran
   * macOS 10.11 El Capitan, R-release (experimental)

## Changes since last release

* Fix CRAN check warnings for dataone 2.1.3 (#241)
* Mark deprecated function as defunct (#240)
* Fix roxygen2 errors related to function arguments (#241)
* remove knitr/RDS dependency on R 3.5
* Fix getDataPackage() failing with "subscript out of bounds" (#243)

## R CMD check results

* There were no NOTEs, ERRORs, or WARNINGs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
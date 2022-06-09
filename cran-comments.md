## Test environments

 * macOS 12.2: R 4.1.0
 * Ubuntu 18.04 R 4.1.3
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R Under development (unstable) (2022-06-08 r82470 ucrt) 
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R 4.2.0 (2022-04-22 ucrt)  
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R 4.1.3 (2022-03-10)
 * rhub::check_for_cran()
   * Windows Server 2008 R2 SP1, R-release, 32/64 bit
   * Windows Server 2022, R-devel, 64 bit
   * Fedora Linux, R-devel, clang, gfortran
   * Fedora Linux, R-devel, GCC
   * macOS 10.13.6 High Sierra, R-release, CRAN's setup
   * Apple Silicon (M1), macOS 11.6 Big Sur, R-release

## Changes since last release

* Remove hash dependency (#293)
* Add support for new method signature for D1Client (#252)
* Ensure rightsHolder persists when uploading a data package (#292)
* Fix bug where 'publc = TRUE' argument did not set public read on all objects (#285)
* Account for edge case errors in 'archive()' (#236)

## R CMD check results

* There were no NOTEs, ERRORs, or WARNINGs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
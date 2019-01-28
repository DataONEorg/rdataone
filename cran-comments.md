## Test environments

* macOS 10.l4 R 3.5.0, R 3.5.2
* Ubuntu 18.10, R 3.5.1
* Debian 9.4, R 3.5.1
* Windows 7, R 3.5.2
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.2 (2018-12-20)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2019-01-27 r76018)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.4.4 (2018-03-15)

## Changes since last release

* Improve error handling for services that call DataONE 'resolve' service (#232)
* Eliminate duplicate entries for package vignettes (#232)

## R CMD check results

* There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
## Test environments

* macOS 10.13.4, R 3.5.0
* Ubuntu 16.04.4, R 3.4.4
* Windows 7, R 3.5.0
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.5.0 (2018-04-23)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2018-06-13 r74894, 2018-06-26 r74934)

## Changes since last release

* Resolve temporary directory problem on Windows (#204)
* Fixed broken links in the 'dataone-overview' vignette (#205)
* Added a destination file path argument to getPackage() (#211)
* Add R package 'xml2' to DESCRIPTION (suggest) to resolve 'unstated dependencies' warning (#218)

NEW FUNCTIONS

* The new method `downloadObject()` provides a way to easily download a file from DataONE to disk (#217)

## R CMD check results

* There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

* The downstream dependency (nesRdata) has been checked with revdepcheck::revdep_check(), which passed
  with 0 errors, 0 warnings, and 1 note. After reviewing the 'nesRdata' source and issues, it appears that
  this is a problem with 'nesRdata' and not 'dataone', and was resolved in this commit in the 'nesRdata'
  github repository: https://github.com/jsta/nesRdata/commit/7fcf99be892e06253219ca7f95603fea968a8f72
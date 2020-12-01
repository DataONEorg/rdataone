## Test environments

 * macOS 10.14: R 4.0.2
 * Ubuntu 18.04 R 3.4.4
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R 4.0.3 (2020-10-10)
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R Under development (unstable) (2020-11-27 r79522) 
 * Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R 3.6.3 (2020-02-29)
 * rhub::check_for_cran()
   * Debian Linux, R-devel, GCC: debian-gcc-devel
   * Fedora Linux, R-devel, GCC: fedora-gcc-devel
   * Fedora Linux, R-devel, clang, gfortran: fedora-clang-devel
   * macOS 10.13.6 High Sierra, R-release, CRAN's setup: macos-highsierra-release-cran

## Changes since last release

* The 'lazyLoad' behavior for 'getDataObject()', 'getDataPackage()' has changed (#258)
* Use 'SHA-256' checksum for sysmeta/object uploads (#257)
* Fixed bug where uploading modified 'metadata-only' package caused error (#256)
* UploadDataPackage now supports 'common' objects (#251)
* Create packages with uniform checksum (#261)
* Update tests for compatibility with testthat 3e (#260)
* Query results returned 'as=data.frame' now contain Solr multi-valued fields as R lists (#250)

## R CMD check results

* There were no NOTEs, ERRORs, or WARNINGs.

## Downstream dependencies

* The downstream dependencies have been checked with revdepcheck::revdep_check(), without any problems being reported.
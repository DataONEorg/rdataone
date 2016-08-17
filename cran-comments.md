## Test environments

* OS X 10.11.6, R 3.3.1
* Ubuntu 14.04, R 3.3.1
* Windows 7, R 3.3.1 (i386, x86_64)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.3.1 (2016-06-21)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R devel (2016-08-16 r71104)

## R CMD check results

* There were no ERRORs or WARNINGs.
* There was 1 NOTE:
  - A NOTE checking CRAN incoming feasibility:
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - API (4:40)
      - DataONE (4:27, 18:9, 18:53)
      - metadata (17:57, 21:34, 22:5)

## Downstream dependencies

* No downstream dependencies currently exist, as reported by devtools::revdep_check()

## Test environments

* OS X 10.10.5, R 3.2.4
* OS X 10.11.3 R 3.2.4
* Ubuntu 14.04, R 3.2.4
* Windows 7, R 3.1.3, R 3.2.4, R 3.3.0 beta (2016-04-17 r70499)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.2.5 (2016-04-14)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.3.0 beta (2016-04-23 r70535)

## R CMD check results

* There were no ERRORs or WARNINGs.
* There were 2 NOTEs:
  - A NOTE checking CRAN incoming feasibility:
    - indicating that this is a new submission
    - indicating that the old dataone package was archived on CRAN
      - it was archived due solely to perceived license issues with Java code, 
        all of which has been removed in this release
    - indicating possible mispelled words in the DESCRIPTION. These have been checked
      and are either valid acronyms or proper names:
      - API (4:40)
      - DataONE (4:27, 18:9, 18:53)
      - metadata (17:57, 21:34, 22:5)
  - A NOTE about the suggested package "PKIplus" that is available throught the
    "Additional_repositories" mechanism from the repository http://NCEAS.github.io.drat. This repo
    includes PKIplus builds for Mac OS X, Windows and a source distribution.

## Downstream dependencies

* No downstream dependencies found using revdep_check()

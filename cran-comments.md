Dear CRAN maintainers,

A revised submission for dataone-2.3.0 with a fix for the test failure on Debian forky R-devel. I can't seem to find a straightforward way to replicate this environment through winbuilder or rhub, but the problem was minor and I think this will fix it. So resubmitting to test again under Debian foky R-devel. It passes on all of the other platforms below still. 

## Test environments

* Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R version 4.5.2 (2025-10-31 ucrt) 
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R Under development (unstable) (2025-12-04 r89100 ucrt)
* macOS Sequoia 15.7.2: aarch64-apple-darwin20 R version 4.5.2 (2025-10-31)
* macOS Ventura 13.7.6: aarch64-apple-darwin20 R version 4.5.2 (2025-10-31)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.5.2 (2025-10-31)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.4.3 (2025-02-28)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R Under development (unstable) (2025-12-02 r89085)

## R CMD check results

* There were no NOTEs, ERRORs, or WARNINGs except for those related to this being a new package submission after archive, and a false positive on a spelling check for a proper noun (DataONE).
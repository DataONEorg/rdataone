Dear CRAN maintainers,

Please find a new release of the R package 'dataone' for your consideration. This package was recently archived because I had been unable to fix issues with TLS 1.3 support on Windows clients. I have temporarily disabled TLS 1.3 support and fallback to TLS 1.2 in the underlying 'curl' package on windows clients, which has resolved the issue. I will continue to investigate a more permanent solution for future releases, but it seems that Windows clients are not yet fully compatible with TLS 1.3 (see https://windowsforum.com/threads/tls-1-3-iis-express-on-windows-11-mtls-breakage-workarounds-and-outlook.379408/).

## Test environments

* Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R version 4.5.2 (2025-10-31 ucrt) 
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit) R Under development (unstable) (2025-12-04 r89100 ucrt)
* macOS Sequoia 15.7.2: aarch64-apple-darwin20 R version 4.5.2 (2025-10-31)
* macOS Ventura 13.7.6: aarch64-apple-darwin20 R version 4.5.2 (2025-10-31)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.5.2 (2025-10-31)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R version 4.4.3 (2025-02-28)
* Ubuntu 24.04.3 LTS: x86_64-pc-linux-gnu R Under development (unstable) (2025-12-02 r89085)

## R CMD check results

* There were no NOTEs, ERRORs, or WARNINGs except for those related to this being a new package submission after archive, and a false positive on a spelling check for a proper noun.
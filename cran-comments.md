## Test environments

* macOS 10.12.6, R 3.4.1
* Ubuntu 14.04.5, R 3.4.1
* Windows 7, R 3.4.1
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R version 3.4.1 (2017-06-30)
* Windows (via win-builder): x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2017-08-02 r73018)


## Changes since last release

* Fixed bug where query() was incorrectly converting date results (#174)

* Fixed bug where query() was returning incorrect results for multi-valued Solr fields (#179)

* Fixed bug where createObject() was not uploading in-memory objects correctly (#198)

* Updated methods to aid in downloading, editing and updating packages in DataONE (#175)

* Added getMetadataMember() to identify the metadata object for a DataPackage (#175)

* Updated getPackage() to accept pids for data or metadata object (#178)

* The resource map for a package is now sets the default name (sysmeta.fileName) based on the metadata pid (#195)

* Unit tests have been modified to run faster.

NEW FUNCTIONS

* getMetadataMember() examines each DataObject in a DataPackage to determine if a metadata object has been added
  to the package and returns it if found.

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

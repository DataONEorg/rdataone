#
# dataone: R interface to the DataONE network of data repositories
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dataone)](https://cran.r-project.org/package=dataone)
[![Build Status](https://travis-ci.org/DataONEorg/rdataone.png?branch=master)](https://travis-ci.org/DataONEorg/rdataone)

- **Authors**: Matthew B. Jones ([NCEAS](http://www.nceas.ucsb.edu)), Peter Slaughter, Rob Nahf, Carl Boettiger, Chris Jones, Jordan Read, Lauren Walker, Edmund Hart, Scott Chamberlain
- [doi:10.5063/F1M61H5X](http://doi.org/10.5063/F1M61H5X)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/rdataone)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/rdataone/issues)

Provides read and write access to data and metadata from the [DataONE network 
    of data repositories](https://www.dataone.org/current-member-nodes), including the
    [KNB Data Repository](https://knb.ecoinformatics.org), [Dryad](http://datadryad.org),
    and the NSF [Arctic Data Center](https://arcticdata.io).
    Each DataONE repository implements a consistent repository application 
    programming interface. Users call methods in R to access these remote 
    repository functions, such as methods to query the metadata catalog, get 
    access to metadata for particular data packages, and read the data objects 
    from the data repository using the global identifier for each data object. 
    Users can also insert and update data objects on repositories that support 
    these methods. For more details, see the vignettes.

## Installation Notes 

Version 2.0 of the *dataone* R package removes the dependency on rJava and significantly changes the base 
API to correspond to the published [DataONE API](https://purl.dataone.org/architecture/apis/index.html).  Previous methods for accessing DataONE will be maintained, but new methods have been added. 

The *dataone* R package requires the R package *redland*. If you are installing on Ubuntu then the Redland C libraries
must be installed first. If you are installing on Mac OS X or Windows then installing these libraries is not required.

### Installing on Mac OS X

On Mac OS X dataone can be installed with the following commands:

```
install.packages("dataone")
library(dataone)
```

The *dataone* R package should be available for use at this point.

Note: if you wish to build the required *redland* package from source before installing *dataone*, please see the redland [installation instructions]( https://github.com/ropensci/redland-bindings/tree/master/R/redland).

### Installing on Ubuntu

For ubuntu, install the required Redland C libraries by entering the following commands 
in a terminal window:

```
sudo apt-get update
sudo apt-get install librdf0 librdf0-dev
```

Then install the R packages from the R console:

```
install.packages("dataone")
library(dataone)
```

The *dataone* R package should be available for use at this point

### Installing on Windows

For windows, the required *redland* R package is distributed as a binary release, so it is not
necessary to install any additional system libraries.

To install the *dataone* R packages from the R console:

```
install.packages("dataone")
library(dataone)
```

The *dataone* R package should be available for use at this point.

Note: if you wish to build the required *redland* package from source before installing *dataone*, please see the redland [installation instructions]( https://github.com/ropensci/redland-bindings/tree/master/R/redland).


## Quick Start

See the full manual for documentation, but once installed, the package can be run in R using:
```
library(dataone)
help("dataone")
```

To search the DataONE Federation Member Node *Knowledge Network for Biocomplexity (KNB)* for a dataset:

```
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")
mySearchTerms <- list(id="doi*hstuar*", abstract="Zostera", keywords="Benthic")
result <- query(mn, searchTerms=mySearchTerms, as="data.frame")
pid <- result[1,'id']
```

A CSV data object can be downloaded from KNB with the commands:

```
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")
dataRaw <- getObject(mn, "df35d.443.1")
dataChar <- rawToChar(dataRaw)
theData <- textConnection(dataChar)
df <- read.csv(theData, stringsAsFactors=FALSE)
```

Uploading a CSV file to a DataONE Member Node requires authentication via CILogon, but is similarly simple::

```
library(datapack)
library(uuid)
d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
id <- paste("urn:uuid:", UUIDgenerate(), sep="")
testdf <- data.frame(x=1:10,y=11:20)
csvfile <- paste(tempfile(), ".csv", sep="")
write.csv(testdf, csvfile, row.names=FALSE)
# Build a DataObject containing the csv, and upload it to the Member Node
d1Object <- new("DataObject", id, format="text/csv", filename=csvfile)
uploadDataObject(d1c, d1Object, public=TRUE)
```

Note that this example uploads a data file to the DataONE test environment "STAGING" and not the production environment ("PROD"), in order to avoid inserting a bunch of test data into the production
network. Users should use "STAGING" for testing, and "PROD" for real data submissions.

## Acknowledgements
Work on this package was supported by:

- NSF-ABI grant #1262458 to C. Gries, M. Jones, and S. Collins.
- NSF-DATANET grants #0830944 and #1430508 to W. Michener, M. Jones, D. Vieglais, S. Allard and P. Cruse

Additional support was provided for working group collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)
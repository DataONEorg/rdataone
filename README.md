# dataone: R interface to the DataONE network of data repositories
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/dataone)](https://cran.r-project.org/package=dataone)
  [![R-CMD-check](https://github.com/DataONEorg/rdataone/workflows/R-CMD-check/badge.svg)](https://github.com/DataONEorg/rdataone/actions)

- **Authors**: Matthew B. Jones ([NCEAS](https://www.nceas.ucsb.edu)), Peter Slaughter, Rob Nahf, Carl Boettiger, Chris Jones, Jordan Read, Lauren Walker, Edmund Hart, Scott Chamberlain
- [doi:10.5063/F1M61H5X](https://doi.org/10.5063/F1M61H5X)
- **License**: [Apache 2](https://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/rdataone)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/rdataone/issues)

Provides read and write access to data and metadata from the [DataONE network 
    of data repositories](https://www.dataone.org/current-member-nodes), including the
    [KNB Data Repository](https://knb.ecoinformatics.org), [Dryad](https://datadryad.org/stash),
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

## Quick Start

See the full manual (`help(dataone)`) for documentation.

To search the DataONE Federation Member Node *Knowledge Network for Biocomplexity (KNB)* for a dataset:

```
library(dataone)
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")
mySearchTerms <- list(q="abstract:salmon+AND+keywords:spawn+AND+keywords:chinook",
                      fl="id,title,dateUploaded,abstract,size",
                      fq="dateUploaded:[2017-06-01T00:00:00.000Z TO 2017-07-01T00:00:00.000Z]",
                      sort="dateUploaded+desc")
result <- query(mn, solrQuery=mySearchTerms, as="data.frame")
result[1,c("id", "title")]
id <- result[1,'id']
```

The metadata file that describes the located research can be downloaded and viewed in an XML viewer, text 
editor after being written to disk, or in R via the commands below:
```
library(XML)
metadata <- rawToChar(getObject(mn, id))
doc <- xmlRoot(xmlTreeParse(metadata, asText=TRUE, trim = TRUE, ignoreBlanks = TRUE))
tf <- tempfile()
saveXML(doc, tf)
file.show(tf)
```

This metadata file describes a data file (CSV) in this data collection (package) that can be obtained using 
the listed identifier, using the commands:

```
dataRaw <- getObject(mn, "urn:uuid:49d7a4bc-e4c9-4609-b9a7-9033faf575e0")
dataChar <- rawToChar(dataRaw)
theData <- textConnection(dataChar)
df <- read.csv(theData, stringsAsFactors=FALSE)
df[1,]
```

Uploading a CSV file to a DataONE Member Node requires user authentication. DataONE user
authentication is described in the vignette `dataone-federation`.

Once the authentication steps have been followed, uploading is done with:
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

In addition, a collection of science metadata and data can be downloaded with one
command, for example:

```
d1c <- D1Client("PROD", "urn:node:KNB")
pkg <- getDataPackage(d1c, id="urn:uuid:04cd34fd-25d4-447f-ab6e-73a572c5d383", quiet=FALSE)
```
See the R vignette [dataone R Package ](https://github.com/DataONEorg/rdataone/blob/master/vignettes/v01-dataone-overview.Rmd) for
more information.

## Acknowledgments
Work on this package was supported by:

- NSF-ABI grant #[1262458](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1262458) to C. Gries, M. B. Jones, and S. Collins.
- NSF-DATANET grants #[0830944](https://www.nsf.gov/awardsearch/showAward?AWD_ID=0830944) and #[1430508](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1430508) to W. Michener, M. B. Jones, D. Vieglais, S. Allard and P. Cruse
- NSF DIBBS grant #[1443062](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1443062) to T. Habermann and M. B. Jones
- NSF-PLR grant #[1546024](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1546024) to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden
- NSF-PLR grant #[2042102](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2042102) to M. B. Jones, A. Budden, J. Dozier, and M. Schildhauer

Additional support was provided for working group collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://live-ncea-ucsb-edu-v01.pantheonsite.io/sites/default/files/2020-03/NCEAS-full%20logo-4C.png)](https://www.nceas.ucsb.edu)

[![dataone_footer](https://www.dataone.org/sites/all/images/DataONE_LOGO.jpg)](https://www.dataone.org)


[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)

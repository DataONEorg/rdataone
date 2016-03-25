#
# dataone: R interface to the DataONE network of data repositories
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dataone)](https://cran.r-project.org/package=dataone)

- **Authors**: Matthew B. Jones ([NCEAS](http://www.nceas.ucsb.edu)), Peter Slaughter, Rob Nahf, Carl Boettiger, Chris Jones, Jordan Read, Lauren Walker, Edmund Hart, and Scott Chamberlain
- [doi:10.5063/F1M61H5X](http://doi.org/10.5063/F1M61H5X)
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/DataONEorg/rdataone)
- [**Submit Bugs and feature requests**](https://github.com/DataONEorg/rdataone/issues)

Provides read and write access to data and metadata from the [DataONE network 
    of data repositories](https://www.dataone.org/current-member-nodes).  
    Each DataONE repository implements a consistent repository application 
    programming interface. Users call methods in R to access these remote 
    repository functions, such as methods to query the metadata catalog, get 
    access to metadata for particular data packages, and read the data objects 
    from the data repository. Users can also insert and update data objects on 
    repositories that support these methods.

## Installation Notes 

The current version of the *dataone* R package removes the dependency on rJava and significantly changes the base 
API to correspond to the published  [DataONE API](https://purl.dataone.org/architecture/apis/index.html).  Previous methods for accessing DataONE will be maintained, but new methods have been added. 

The `redland` C library must be installed on your OS prior to installing the R package. 

## Installing development versions of the package
To obtain builds of the *dataone* R package before they are available from CRAN, development builds
can be obtained using the NCEAS repository.  The NCEAS repository is maintained and accessed using the 
R package *drat*.

The R package *drat* allows an R package provider to easily make repositories available that are searched with the standard R 
commands such as `available.packages()`, `install.packages()`, `update.packages()`, etc.

In order to obtain development versions of the *dataone* package, the the NCEAS drat repository can be included 
in the repository search list by entering these commands at the R console:

```
install.packages("drat")
library(drat)
addRepo("NCEAS")
```

Once the NCEAS drat repository has been added, then the *dataone* package can be installed using the installation
instructions for one of the target platforms: Mac OS X, Ubuntu, Windows.

To disable installation of *dataone* development builds from the NCEAS drat repository:
```
detach("package:drat", unload=TRUE)
```

## Installing on Mac OS X

On Mac OS X, the required Redland C libraries can be installed with either [Mac Ports](https://www.macports.org) package manager
or the [HomeBrew](http://brew.sh) package manager. The HomeBrew package manager can be significantly faster to install
but either one will work provided the directions shown below are followed.

You can check if you have MacPorts installed by entering the following command in a terminal window:

```
port version
```

### Mac OS X install option 1: install using Macports
If you are already using the MacPorts package manager, you can install *dataone* with the following commands, 
otherwise, it is recommended that you skip to the next section *Installing with HomeBrew*. To install
the *dataone* R package with MacPorts, first enter this command at a terminal window:

```
sudo port install redland
```

Then enter these commands in the R console:

```
install.packages("redland", type="source")
install.packages("dataone")
```

Please note that the *install.packages* command specifies a "source" installation. Installing from
source is only necessary if Macports is being used, and is not a requirement if Homebrew is used.

The *dataone* R package should be available for use at this point.

### Mac OS X install option 2: install using HomeBrew
On Mac OS X you can use the package management system [HomeBrew](http://brew.sh) to install the 
necessary libraries. The HomeBrew software can be installed with the following command entered at a terminal window:

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Once HomeBrew has been installed, you can then enter the following command to install the Redland C libraries:

```
brew install redland
```

Next, install the *dataone* R package with these commands typed at the R console:

```
install.packages("dataone")
library(dataone)
```
  
The *dataone* R package should be available for use at this point.

## Installing on Ubuntu

For ubuntu, install the required Redland C libraies:

```
sudo apt-get update
sudo apt-get install librdf0 librdf0-dev
```

Then install the R package from the R console:

```
install.packages("dataone")
library(dataone)
```
  
The *dataone* R package should be available for use at this point.

## Installing on Windows

For windows, the redland R package is distributed as a binary release, and it is not necessary to install any 
additional system libraries.

To install the R package from the R console:

```
install.packages("dataone")
library(dataone)
```

The *dataone* R package should be available for use at this point.

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
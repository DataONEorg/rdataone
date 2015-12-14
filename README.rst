.. image:: https://travis-ci.org/ropensci/rdataone.svg?branch=master
    :target: https://travis-ci.org/ropensci/rdataone

DataONE R Client
================

An R_ package that provides read/write access to data and metadata from the DataONE_ network of `Member Node`_ data repositories. Member Nodes in DataONE are independent data repositories that have adopted the DataONE services for interoperability, making each of the repositories accessible to client tools such as the DataONE R Client using a standard interface.  The DataONE R Client can be used to access data files and to write new data and metadata files to nodes in the DataONE network.  

.. _R: http://www.r-project.org/
.. _DataONE: http://www.dataone.org/
.. _Member Node: http://www.dataone.org/member-nodes

Downloads
---------

The currently supported version requires two R packages, dataone and dataonelibs (which provides the Java libraries necessary for the dataone package to function):

- DataONE R Client (dataone_1.0.0.tar.gz_)
- DataONE Libraries (dataonelibs_1.0.0.tar.gz_)

Documentation for the DataONE R Client is provided as help files within the R framework, and is downloadable in PDF format:

- DataONE R Client Manual (dataone_1.0.0-manual.pdf_)

.. _dataone_1.0.0.tar.gz: https://releases.dataone.org/dist/dataone_R/dataone_1.0.0.tar.gz

.. _dataonelibs_1.0.0.tar.gz: https://releases.dataone.org/dist/dataone_R/dataonelibs_1.0.0.tar.gz

.. _dataone_1.0.0-manual.pdf: https://releases.dataone.org/dist/dataone_R/dataone_1.0.0-manual.pdf

Installation Notes
------------------

The easiest way to install the client is via the GitHub_ repository::


  > devtools::install_github("DataONEorg/rdataone/dataonelibs", ref="D1_CLIENT_R_v1.0.0")

  > devtools::install_github("DataONEorg/rdataone/dataone", ref="D1_CLIENT_R_v1.0.0")


.. _CRAN: http://cran.r-project.org

Alternatively, you can download and install the package manually from this web
site above and, after downloading the package, install it using::

  $ R CMD install dataonelibs_1.0.0.tar.gz

  $ R CMD install dataone_1.0.0.tar.gz

This package uses rJava to bind to the Java client libraries.  Be sure to install 
rJava for your particular version of R and Java.

.. > install.packages("rJava",,"http://rforge.net/",type="source")

Quick Start (Version 1)
-----------------------

See the full manual for documentation, but once installed, the package can be run in R using::

  $ R 
  > library(dataone)
  Loading required package: rJava
  Loading required package: XML
  Loading required package: dataonelibs

  > help(dataone)

Reading a CSV data file from a DataONE node is as simple as::
  
  > library(dataone)
  > cli <- D1Client()
  > d1o <- getD1Object(cli, "doi:10.5063/AA/hstuar01.10.1")
  > mydf <- asDataFrame(d1o)

Writing a CSV file to a DataONE Member Node requires authentication via CILogon, but is similarly simple::

  > library(uuid)
  > mn_nodeid <- "urn:node:mnTestKNB"
  > cli <- D1Client("STAGING2")
  > id <- paste("urn:uuid:", UUIDgenerate(), sep="")
  > testdf <- data.frame(x=1:10,y=11:20)
  > csvdata <- convert.csv(cli, testdf)
  > format <- "text/csv"

  ## Build a D1Object containing the csv, and upload it to the MN
  > d1Object <- new(Class="D1Object", id, csvdata, format, mn_nodeid)
  > setPublicAccess(d1Object)
  > createD1Object(cli, d1Object)


Version 2 Release Candidate
---------------------------

We are currently working on a `version 2 release milestone`_ that removes the dependency on rJava.  
This work is partially complete and significantly changes the base API to correspond to the published 
DataONE API.  Previous methods for accessing DataONE will be maintained, but new methods will be added.
Consequently, the current development snapshot in the master branch is quite different from the 1.0.0
release.  Be aware that there are API differences that are not fully finalized.

.. _version 2 release milestone: https://github.com/DataONEorg/rdataone/milestones/2.0.0

Because the v2 package has not been released to CRAN, nor have its dependencies, you need to install the 
dependencies manually before installing the package itself.  The main dependency is the redlands librdf 
binary, which must be installed on your OS prior to installing the R code::

Before the `redland` R package can be installed, the redland C libraries must be installed.

On Mac OSX you can use the package management system [HomeBrew](http://brew.sh) to install the necessary libraries. The HomeBrew
software can be installed with the following command entered at a terminal window:

```
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Once HomeBrew has been installed, you can then enter the following command to install the Redland C libraries:

```
brew install redland
```


  
  # For ubuntu, use the appropriate .deb packages
  $ sudo apt-get install librdf0 librdf0-dev


Once redland is installed, using the CRAN 'drat' package and the NCEAS repository, one can install 
all of the R dependencies needed using drat and the install.packages() 

 ```
  $ R
  > install.packages("drat")
  > library(drat)
  > addRepo("NCEAS")
  > install.packages("dataone")
 ``` 

Development logs
----------------
For developers interested in getting an email for each push to the rdataone repository, you can subscribe to our mailing list:
    
    rdataone-dev: `List Info`_
    
.. _List Info: http://lists.dataone.org/mailman/listinfo/rdataone-dev/


License
-------

The DataONE R Client is licensed as open source software under the Apache 2.0 license.

Authors
-------

- Matthew Jones <jones@nceas.ucsb.edu>
- Rob Nahf <nahf@dataone.unm.edu>
- Chris Jones <cjones@nceas.ucsb.edu>
- Carl Boettiger <cboettig@gmail.com>
- Lauren Walker <walker@nceas.ucsb.edu>
- Scott Chamberlain <myrmecocystus@gmail.com>
- Edmund Hart <edmund.m.hart@gmail.com>
- Jordan Read <jread@usgs.gov>

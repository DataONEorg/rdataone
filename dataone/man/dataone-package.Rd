\name{dataone-package}
\alias{dataone-package}
\alias{dataone}
\docType{package}
\title{
DataONE R Client
~~ package title ~~
}
\description{
A package that provides read/write access to data and metadata from 
the DataONE network of Member Node data repositories.

}
\details{
\tabular{ll}{
Package: \tab dataone\cr
Version: \tab 1.0.0\cr
Date: \tab 2013-02-14\cr
Depends: \tab R (>= 2.9.2), rJava (>= 0.8-5), XML (>= 3.95-0.1), methods\cr
Imports: \tab rJava, XML\cr
SystemRequirements: \tab Java (>= 1.6)\cr
License: \tab file LICENSE\cr
Packaged: \tab 2013-02-14 20:54:27 UTC; jones\cr
Repository: \tab CRAN\cr
Date/Publication: \tab 2013-02-14 20:49:45\cr
URL: \tab http://www.dataone.org/investigator-toolkit/dataone_r\cr
BugReports: \tab https://redmine.dataone.org\cr
Built: \tab R 2.15.1; ; 2013-02-15 21:42:52 UTC; unix\cr
}

Index:
\preformatted{
AbstractTableDescriber-class
                        Class '"AbstractTableDescriber"'
CertificateManager-class
                        Class '"CertificateManager"'
D1Client-class          Class '"D1Client"'
D1Object-class          Class '"D1Object"'
DataPackage-class       Class '"DataPackage"'
EMLParser-class         Class '"EMLParser"'
dataone-package         Package to access data and metadata in the
                        DataONE repository federation.
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~

This package provides facilities to manage data and metadata within the DataONE
  federation of data repositories.  DataONE defines four tiers of standard
  services that can be implemented by participating repositories, called
  Member Nodes.  These services includes the ability to search and discover data
  and metadata objects across the whole federation, retrieve objects from 
  Member Nodes, and write results and metadata to Member Nodes.  The latter functions
  require user authentication to function properly.  The dataone R package provides
  a set of convenience classes and functions for accessing these DataONE services.

This package defines 3 main classes: \code{D1Client}, \code{D1Object}, and \code{DataPackage}. 
  The D1Client object represents the DataONE system, and is used to interact with DataONE.
  A D1Object is a discrete data-object with a unique identifier in DataONE.
  A DataPackage is simply a collection of related D1Objects with the relationships
  defined in a resourceMap.
  
  Users retrieve D1Objects or DataPackages through D1Client methods, and 
  conversely create D1Objects or DataPackages using other D1Client methods.
  
  \bold{Security}:
  DataONE identifies users with x509 certificates issued by CILogon.org.  Without
  a certificate installed on your local machine, you will not have access to any 
  content not explicitly made public, nor can you submit new content or make any
  content modifications.
  
  The package also defines a CertificateManager package for managing your client 
  certificate.  CILogon certificates are short-lived, lasting less than 24 hours, 
  so typically you will want to download a fresh certificate the first time you 
  interact with DataONE each day.

}
\author{
Matthew Jones, Rob Nahf

Maintainer: Matthew Jones <jones@nceas.ucsb.edu>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
 \dontrun{
    library(dataone)
    d1 <- D1Client("DEV")
    dataPack <-getDataPackage(d1,"someknownPackageID")
    d1object <-getMember(dataPack,"someKnownMemberID") 
  }
}

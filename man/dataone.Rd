\name{dataone}
\alias{dataone-package}
\docType{package}
\title{Package to access data and metadata in the DataONE repository federation.}
\description{
  This package provides facilities to manage data and metadata within the DataONE
  federation of data repositories.  DataONE defines four tiers of standard
  services that can be implemented by participating repositories, called
  Member Nodes.  These services includes the ability to search and discover data
  and metadata objects across the whole federation, retrieve objects from 
  Member Nodes, and write results and metadata to Member Nodes.  The latter functions
  require user authentication to function properly.  The dataone R package provides
  a set of convenience classes and functions for accessing these DataONE services.
}
\details{
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
\author{Matthew Jones, Rob Nahf}
\examples{
  \dontrun{
    library(dataone)
    d1 <- D1Client("DEV")
    dataPack <-getDataPackage(d1,"someknownPackageID")
    d1object <-getMember(dataPack,"someKnownMemberID") 
  }
}
\keyword{data}
\keyword{manip}

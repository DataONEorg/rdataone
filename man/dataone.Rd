\name{dataone}
\alias{dataone-package}
\title{Package to access data and metadata in the DataONE repository federation.}
\description{
  This package provides facilities to manage data and data within the DataONE
  federation of data repositories.  DataONE defines four tiers of standard
  services that can be implemented by participating repositories, called
  Member Nodes.  These services includes the ability to search and discover data
  and metadata objects across the whole federation, retrieve objects from particular
  Member Nodes, and write results and metadata to Member Nodes.  The latter functions
  require user authentication to function properly.  The dataone R package provides
  a set of convenience classes and functions for accessing these DataONE services.
}
\usage{
  library(dataone)
  d1 <- D1Client("DEV")
  # Configurable settings for these tests
  #mn_nodeid <- "urn:node:DEMO1"
  #sleep_seconds <- 30
  #dp <- getD1Object(d1, id)
  #mydf <- asDataFrame(dp,1)
#
  #cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
  #id <- paste("r:test", cur_time, "1", sep=".")
   
  # Create a DataONE client, and login
  #d1 <- D1Client()

  # Create a data table, write it to csv format, and insert into DataONE
  #testdf <- data.frame(x=1:10,y=11:20)
  #csvdata <- convert.csv(d1, testdf)
  #format <- "text/csv"
  #d1object <- createD1Object(d1, id, csvdata, format, mn_nodeid)
  #newId <- d1object$getIdentifier()
  #d1object$setPublicAccess(d1@session)
  #d1object$create(d1@session)
  #if (!is.null(e<-.jgetEx())) {
      #print("Java exception was raised")
      #print(.jcheck(silent=TRUE))
  #}
}
\details{
  Most interaction with DataONE services requires the client to have a valid X509 
  certificate.  DataONE uses CILogon.org to issue client certificates, which is
  accessed at https://cilogon.org/?skin=DataONE.  
  
  To successfully use the dataone R package, users first need to install a CILogon
  certificate on their local machine.  These CILogon certificates are short-lived,
  so typically, a user will need to download a fresh certificate each day.
  
  CILogon recognizes several identity providers, including Google, so most times
  new users to DataONE can get certificates using existing institutional accounts.
  For more information about the CILogon service, see http://www.cilogon.org/faq
}
\author{Matthew Jones}
\examples{
  library(dataone)
  d1 <- D1Client("DEV")
}
\keyword{data}
\keyword{manip}

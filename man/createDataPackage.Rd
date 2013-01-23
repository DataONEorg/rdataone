\name{createDataPackage}
\alias{createDataPackage}
\title{
  createDataPackage
}
\description{
  Creates the D1Objects contained in the DataPackage by calling the createD1Object()
  on each of the members, as well as assembling the resourceMap object from the
  recorded relationships, and calling create() on it as well.
}
\usage{
createDataPackage(x, d1Object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The D1Client that will do the creating
}
  \item{dataPackage}{
  The DataPackage instance to be submitted to DataONE for creation.  
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
  The DataPackage describes the collection of data object and their associated 
  metadata object, with the relationships and members serialized into a document
  stored under, and retrievable with, the packageId as it's own distinct object.

  Members are created serially, and most errors in creating one object will 
  interrupt the create process for the whole, and only some members will get created.
  
  If a member of a DataPackage is a pre-existing object in DataONE, an exception
  will be raised,
  raised by it's creation will 
  
  
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  See d1_libclient_java documentation D1Client.create()
  	\url{"http://dev-testing.dataone.org:8080/hudson/job/d1_libclient_java/ws/d1_libclient_java/target/site/apidocs/org/dataone/client/D1Client.html#create(org.dataone.service.types.v1.Session,%20org.dataone.client.D1Object)"}
}
\author{
	Rob Nahf
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  \dontrun{
  # (not executable!!)
  library(dataone)
  cli <- D1Client("STAGING","urn:node:memberNodeFoo")
  
  # a fictitious function that assembles a D1Object
  d1object1 <- makeANewDataObject()
  d1object2 <- makeANewDataObject()
  d1objectMeta <- makeAMetadataObject(d1object1,d1object2)
  
  dataPackage <- new("DataPackage",packageId="aBrandNewPackage")
  addMember(dataPackage, d1object1)
  addMember(dataPackage, d1object2)
  addRelationship(dataPackage,d1objectMeta,c(d1object1,d1object2))
  
  createDataPackage(cli, dataPackage)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}
\keyword{manip}

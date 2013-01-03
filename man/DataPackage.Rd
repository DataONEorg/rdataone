\name{DataPackage}
\alias{DataPackage}
\title{
  The DataPackage class     
}
\description{
  An object representing a data package in DataONE, which consists of a
  series of data objects, the science metadata objects that documents them, 
  and the resourceMap object that defines the relationships.
}
\usage{
new("DataPackage", packageId = pid)
new("DataPackage", jDataPackage = jDataPackage)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{identifier}{
    The identifier for the data package, that corresponds to the DataONE identifier
    for the resourceMap object. If a new package is being built, this should be
    a new identifier not already registered in DataONE.
  }
  \item{jDataPackage}{
    A jJobRef to an instantiated java DataPackage object.  This parameter is typically
    only used when retrieving an existing package from the DataONE system, as through
    the D1Client's 'getPackage' method.
  }
}
\details{
  A DataPackage object is a thin wrapper around the Java org.dataone.client.DataPackage
  class, exposing most methods, and adapting their parameters to remove the need
  to provide java-specific instances.  
  
  At least one method, "asDataFrame," is completely implemented within this package
  and has no corresponding java method.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
  ## example of instantiating a new DataPackage
  data_package <- new(Class="DataPackage",packageId="somePackageId")
  
  ## example of instantiating an existing DataPackage, through the
  ## fictional intermediate function foo()
  jD1Package <- foo()
  data_package <- new(Class="DataPackage", jDataPackage=jD1Package)
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{DataPackage-class}
\Rdversion{1.1}
\docType{class}
\alias{DataPackage-class}
\alias{addAndDownloadData,DataPackage,character-method}
\alias{addData,DataPackage,D1Object-method}
\alias{asDataFrame,DataPackage,character-method}
\alias{contains,DataPackage,character-method}
\alias{createDataPackage,D1Client,DataPackage-method}
\alias{getData,DataPackage,character-method}
\alias{getIdentifiers,DataPackage-method}
\alias{getMember,DataPackage,character-method}
\alias{getSize,DataPackage-method}
\alias{insertRelationship,DataPackage,character,character-method}
\alias{removeMember,DataPackage,character-method}

\title{Class \code{"DataPackage"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
  An object representing a data package in DataONE, which consists of a
  series of data objects, the science metadata objects that documents them, 
  and the resourceMap object that defines the relationships.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DataPackage", packageId, jDataPackage)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{packageId}:}{Object of class \code{"character"} 
    The identifier for the data package, that corresponds to the DataONE identifier
    for the resourceMap object. If a new package is being built, this should be
    a new identifier not already registered in DataONE. }
    \item{\code{jDataPackage}:}{Object of class \code{"jobjRef"} 
    A jJobRef to an instantiated java DataPackage object.  This parameter is typically
    only used when retrieving an existing package from the DataONE system, as through
    the D1Client's 'getPackage' method.
    }
  }
}
\section{Methods}{
  \describe{
    \item{addAndDownloadData}{\code{signature(x = "DataPackage", identifier = "character")}: ... }
    \item{addData}{\code{signature(x = "DataPackage", d1object = "D1Object")}: ... }
    \item{asDataFrame}{\code{signature(x = "DataPackage", reference = "character")}: ... }
    \item{contains}{\code{signature(x = "DataPackage", identifier = "character")}: ... }
    \item{createDataPackage}{\code{signature(x = "D1Client", dataPackage = "DataPackage")}: ... }
    \item{getData}{\code{signature(x = "DataPackage", id = "character")}: ... }
    \item{getIdentifiers}{\code{signature(x = "DataPackage")}: ... }
    \item{getMember}{\code{signature(x = "DataPackage", identifier = "character")}: ... }
    \item{getSize}{\code{signature(x = "DataPackage")}: ... }
    \item{insertRelationship}{\code{signature(x = "DataPackage", metadataID = "character", dataIDs = "character")}: ... }
    \item{removeMember}{\code{signature(x = "DataPackage", identifier = "character")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
  A DataPackage object is a thin wrapper around the Java org.dataone.client.DataPackage
  class, exposing most methods, and adapting their parameters to remove the need
  to provide java-specific instances.  
  
  At least one method, "asDataFrame," is completely implemented within this package
  and has no corresponding java method.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("DataPackage")

\dontrun{
    ## example of instantiating a new DataPackage
    data_package <- new(Class="DataPackage",packageId="somePackageId")
  
    ## example of instantiating an existing DataPackage, through the
    ## fictional intermediate function foo()
    jD1Package <- foo()
    data_package <- new(Class="DataPackage", jDataPackage=jD1Package)
  }
}
\keyword{classes}

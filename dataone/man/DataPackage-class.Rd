\name{DataPackage-class}
\Rdversion{1.1}
\docType{class}
\alias{DataPackage}
\alias{addAndDownloadData}
\alias{addData}
%%\alias{asDataFrame}
\alias{contains}
%%\alias{createDataPackage}
\alias{getData}
\alias{getIdentifiers}
\alias{getMember}
\alias{getSize}
\alias{insertRelationship}
\alias{removeMember}
\alias{DataPackage-class}
\alias{addAndDownloadData,DataPackage,character-method}
\alias{addData,DataPackage,D1Object-method}
\alias{asDataFrame,DataPackage,character-method}
\alias{contains,DataPackage,character-method}
%%\alias{createDataPackage,D1Client,DataPackage-method}
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
    \item{addAndDownloadData}{\code{signature(x = "DataPackage", identifier = "character")}: 
    downloads a pre-existing D1Object to the DataPackage, using the provided 
    identifier string to retrieve from the DataONE system, and adds it to the 
    local representation.
    }
    \item{addData}{\code{signature(x = "DataPackage", d1object = "D1Object")}: 
    Add the D1Object to the DataPackage's object store, usually in preparation 
	for upcoming submission.
    }
    \item{asDataFrame}{\code{signature(x = "DataPackage", reference = "character")}: 
    return the content of the referenced DataPackage member as a dataFrame, using 
    parsing instructions contained in the object's metadata.  The metadata found via
    the DataPackage resource map. 
    }
    \item{contains}{\code{signature(x = "DataPackage", identifier = "character")}: 
    Returns true if the specified object is a member of the package
    }
    \item{createDataPackage}{\code{signature(x = "D1Client", dataPackage = "DataPackage")}: 
    ... 
    }
    \item{getData}{\code{signature(x = "DataPackage", id = "character")}: 
    Returns the content of the DataPackage member identified by id. see additional
    implementation in \code{link{D1Object-class}}
    }
    \item{getIdentifiers}{\code{signature(x = "DataPackage")}: 
    Return the identifiers of the package members, as defined by the ResourceMap
    }
    \item{getMember}{\code{signature(x = "DataPackage", identifier = "character")}: 
    Given the identifier of a member of the data package, return the D1Object 
    representation of the member.
    }
    \item{getSize}{\code{signature(x = "DataPackage")}: 
    Get the count of D1Objects in the DataPackage
    }
    \item{insertRelationship}{\code{signature(x = "DataPackage", metadataID = "character", dataIDs = "character")}: 
    associate data Objects to the science metadata objects that describe them.  Note 
    that since the resource map that defines a package is separate from the items 
    it associates, it is possible to use identifiers that have not been defined 
    as members of the package, though not recommended.
     }
    \item{removeMember}{\code{signature(x = "DataPackage", identifier = "character")}: 
    removes a D1Object from the object map. 
    }
	 }
}
\author{
  Matthew Jones and Rob Nahf
}
\note{
%%  ~~further notes~~
  A DataPackage object is a thin wrapper around the Java org.dataone.client.DataPackage
  class, exposing most methods, and adapting their parameters to remove the need
  to provide java-specific instances.  
  
  At least one method, "asDataFrame," is completely implemented within this package
  and has no corresponding java method.
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

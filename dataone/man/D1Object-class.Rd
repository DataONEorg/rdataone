\name{D1Object-class}
\Rdversion{1.1}
\docType{class}
\alias{D1Object}
%%\alias{addData}
\alias{asDataFrame}
\alias{canRead}
%%\alias{createD1Object}
%%\alias{EMLParser}
%%\alias{getData}
\alias{getFormatId}
\alias{getIdentifier}
\alias{setPublicAccess}
\alias{D1Object-class}
%%\alias{addData,DataPackage,D1Object-method}
\alias{asDataFrame,D1Object,AbstractTableDescriber-method}
\alias{asDataFrame,D1Object,ANY-method}
\alias{asDataFrame,D1Object,D1Object-method}
\alias{canRead,D1Object,character-method}
%%\alias{createD1Object,D1Client,D1Object-method}
\alias{EMLParser,D1Object-method}
\alias{getData,D1Object,ANY-method}
\alias{getFormatId,D1Object-method}
\alias{getIdentifier,D1Object-method}
\alias{setPublicAccess,D1Object-method}

\title{Class \code{"D1Object"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
This class encapsulates a DataONE object and it's associated systemMetadata, and
provides methods for building one for submission and methods for getting at its
data and system metadata.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("D1Object", id, data, format, mnNodeId)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{jD1o}:}{Object of class \code{"jobjRef"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{addData}{\code{signature(x = "DataPackage", d1object = "D1Object")}: 
	Add the D1Object to the DataPackage's object store, usually in preparation 
	for upcoming submission.
	}
    \item{asDataFrame}{\code{signature(x = "D1Object", reference =
	"AbstractTableDescriber")}: 
	return the D1Object's content as a dataFrame, using parsing instructions 
	from the specific AbstractTableDescriber. see addition method implementation
	in \code{\link{DataPackage-class}}
	}
    \item{asDataFrame}{\code{signature(x = "D1Object", reference = "ANY")}: 
	return the D1Object's content as a dataFrame  
	}
    \item{asDataFrame}{\code{signature(x = "D1Object", reference =
	"D1Object")}: 
	return the D1Object's content as a dataFrame, using parsing instructions 
	contained in the reference metadata D1Object. 
    }
    \item{canRead}{\code{signature(x = "D1Object", subject =
	"character")}: 
	returns TRUE if the subject provided has read permission on the D1Object, based
	on the local copy of the D1Object's AccessPolicy 
    }
    \item{createD1Object}{\code{signature(x = "D1Client", d1Object =
	"D1Object")}: 
	Creates a D1Object on the MemberNode determined by the object's systemMetadata.
	}
    \item{EMLParser}{\code{signature(d1Object = "D1Object")}: 
    Return an EMLParser object for the given D1Object 
    }
    \item{getData}{\code{signature(x = "D1Object", id = "ANY")}: 
    Returns the D1Object's content 
    }
    \item{getFormatId}{\code{signature(x = "D1Object")}: 
    Get the format Identifier of the D1Object 
    }
    \item{getIdentifier}{\code{signature(x = "D1Object")}: 
    Get the Identifier for the D1Object 
    }
    \item{setPublicAccess}{\code{signature(x = "D1Object")}: 
    Grant read permission to the public on this D1Object, changing the system 
    metadata locally.  To be called before createD1Object(), otherwise it will
     not have any real affect.
    }
	}
}
\author{
  Matthew Jones and Rob Nahf
}
\note{
%%  ~~further notes~~
As of Feb 2013, there is only one AbstractTableDescriber subclass
  for parsing metadata: EMLParser.  It can handle EML version
  2.0.0 - 2.1.1 formatted metadata files. Other parsers should become 
  available as separate packages that can be loaded as needed.
}
\examples{
showClass("D1Object")


\dontrun{
  ## asDataFrame
  df <- asDataFrame(data.package, dataMember.id)

  df <- asDataFrame(data.object, its.metadata)

  table.describer <- EMLParser(its.metadata)
  df <- asDataFrame(data.object, table.describer)

  df <- asDataFrame(data.object, sep="\\t", ...)

  df <- asDataFrame(data.object)
}
}
\keyword{classes}

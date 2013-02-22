\name{AbstractTableDescriber-class}
\Rdversion{1.1}
\docType{class}
\alias{AbstractTableDescriber-class}
\alias{data.characterEncoding}
\alias{data.formatFamily}
\alias{data.tableAttributeNames}
\alias{data.tableAttributeOrientation}
\alias{data.tableAttributeStorageTypes}
\alias{data.tableAttributeTypes}
\alias{data.tableFieldDelimiter}
\alias{data.tableMissingValueCodes}
\alias{data.tableQuoteCharacter}
\alias{documented.d1Identifiers}
\alias{documented.d1FormatIds}
\alias{documented.entityNames}
\alias{documented.sizes}
%%\alias{asDataFrame,D1Object,AbstractTableDescriber-method}

\title{Class \code{"AbstractTableDescriber"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
A virtual class (interface) that defines the methods used by the corresponding 
asDataFrame method to get parsing instructions. 

Classes that inherit from this class provide the format-specific ways to
read the metadata.


}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
  \describe{
    \item{asDataFrame}{\code{signature(x = "D1Object", reference = "AbstractTableDescriber")}: ... }
	 }
}

\section{Generics}{
 \describe{
    \item{data.characterEncoding}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    return the character encoding entry from the metadata 
    }
    \item{data.formatFamily}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    return the formatFamily of the data object being described.  asDataFrame methods only work
    on 'text/simpleDelimited'
    }
    \item{data.tableAttributeNames}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns the attribute names defined in the metadata document for the specified data table 
    }
    \item{data.tableAttributeOrientation}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns either "rows" or "columns", based on which way the attribute headers run.  Most data has a
  	header row where the attribute names go across "columns", in which case, the return value for this method
    should be "columns." 
    }
    \item{data.tableAttributeStorageTypes}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns the attributes' data storage types defined in the metadata document for the specified data table 
    }
    \item{data.tableAttributeTypes}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns the attributes' data types defined in the metadata document for the specified data table 
    }
    \item{data.tableFieldDelimiter}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    the field delimiter(s) of the data object being described
	}
    \item{data.tableMissingValueCodes}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns the missing value codes defined in the metadata document for the specified data table 
    }
    \item{data.tableQuoteCharacter}{\code{signature(x = "AbstractTableDescriber", index = "numeric")}: 
    returns the quote characters(s) for the data object being described 
    }
    \item{documented.d1Identifiers}{\code{signature(x = "AbstractTableDescriber")}:
    return a vector of dataONE identifiers associated with each table
    }
    \item{documented.entityNames}{\code{signature(x = "AbstractTableDescriber")}: 
    return a vector of entity names associated with each table
    }
    \item{documented.sizes}{\code{signature(x = "AbstractTableDescriber")}: 
    return a vector of data table sizes (in bytes) 
    }
 }
}
\author{
Matthew Jones and Rob Nahf
}
\examples{
showClass("AbstractTableDescriber")
}
\keyword{classes}

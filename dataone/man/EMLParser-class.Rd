\name{EMLParser-class}
\Rdversion{1.1}
\docType{class}
\alias{EMLParser}
\alias{EMLParser-class}
\alias{data.characterEncoding,EMLParser,numeric-method}
\alias{data.formatFamily,EMLParser,numeric-method}
\alias{data.tableAttributeNames,EMLParser,numeric-method}
\alias{data.tableAttributeOrientation,EMLParser,numeric-method}
\alias{data.tableAttributeStorageTypes,EMLParser,numeric-method}
\alias{data.tableAttributeTypes,EMLParser,numeric-method}
\alias{data.tableFieldDelimiter,EMLParser,numeric-method}
\alias{data.tableMissingValueCodes,EMLParser,numeric-method}
\alias{data.tableQuoteCharacter,EMLParser,numeric-method}
\alias{documented.d1Identifiers,EMLParser-method}
\alias{documented.entityNames,EMLParser-method}
\alias{documented.sizes,EMLParser-method}

\title{Class \code{"EMLParser"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
Class that implements methods to provide parsing instructions for
  asDataFrame for EML metadata documents v2.0.0 - v.2.1.1
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EMLParser", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{d1Object}:}{Object of class \code{"D1Object"} ~~ }
    \item{\code{xmlDocRoot}:}{Object of class \code{"XMLNode"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{AbstractTableDescriber}"}, directly.
}
\section{Methods}{
  \describe{
    \item{data.characterEncoding}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.formatFamily}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableAttributeNames}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableAttributeOrientation}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableAttributeStorageTypes}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableAttributeTypes}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableFieldDelimiter}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableMissingValueCodes}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{data.tableQuoteCharacter}{\code{signature(x = "EMLParser", index = "numeric")}: ... }
    \item{documented.d1Identifiers}{\code{signature(x = "EMLParser")}: ... }
    \item{documented.entityNames}{\code{signature(x = "EMLParser")}: ... }
    \item{documented.sizes}{\code{signature(x = "EMLParser")}: ... }
	 }
}
\author{
  Matthew Jones and Rob Nahf
}
\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
  \code{\linkS4class{AbstractTableDescriber}} for method descriptions 
}
\examples{
showClass("EMLParser")
}
\keyword{classes}

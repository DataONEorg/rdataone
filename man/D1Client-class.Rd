\name{D1Client-class}
\Rdversion{1.1}
\docType{class}
\alias{D1Client-class}
\alias{convert.csv,D1Client-method}
\alias{createD1Object,D1Client,D1Object-method}
\alias{createDataPackage,D1Client,DataPackage-method}
\alias{d1IdentifierSearch,D1Client,character-method}
\alias{d1SolrQuery,D1Client,character-method}
\alias{d1SolrQuery,D1Client,list-method}
\alias{getCN,D1Client-method}
\alias{getD1Object,D1Client-method}
\alias{getEndpoint,D1Client-method}
\alias{getMN,D1Client,ANY-method}
\alias{getMN,D1Client,character-method}
\alias{getMNodeId,D1Client-method}
\alias{getPackage,D1Client,character-method}
\alias{listMemberNodes,D1Client-method}
\alias{reserveIdentifier,D1Client,character-method}
\alias{setMNodeId,D1Client,character-method}

\title{Class \code{"D1Client"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("D1Client", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{endpoint}:}{Object of class \code{"character"} ~~ }
    \item{\code{mn.nodeid}:}{Object of class \code{"character"} ~~ }
    \item{\code{client}:}{Object of class \code{"jobjRef"} ~~ }
    \item{\code{session}:}{Object of class \code{"jobjRef"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{convert.csv}{\code{signature(x = "D1Client")}: ... }
    \item{createD1Object}{\code{signature(x = "D1Client", d1Object = "D1Object")}: ... }
    \item{createDataPackage}{\code{signature(x = "D1Client", dataPackage = "DataPackage")}: ... }
    \item{d1IdentifierSearch}{\code{signature(x = "D1Client", solrQuery = "character")}: ... }
    \item{d1SolrQuery}{\code{signature(x = "D1Client", solrQuery = "character")}: ... }
    \item{d1SolrQuery}{\code{signature(x = "D1Client", solrQuery = "list")}: ... }
    \item{getCN}{\code{signature(x = "D1Client")}: ... }
    \item{getD1Object}{\code{signature(x = "D1Client")}: ... }
    \item{getEndpoint}{\code{signature(x = "D1Client")}: ... }
    \item{getMN}{\code{signature(x = "D1Client", nodeid = "ANY")}: ... }
    \item{getMN}{\code{signature(x = "D1Client", nodeid = "character")}: ... }
    \item{getMNodeId}{\code{signature(x = "D1Client")}: ... }
    \item{getPackage}{\code{signature(x = "D1Client", identifier = "character")}: ... }
    \item{listMemberNodes}{\code{signature(x = "D1Client")}: ... }
    \item{reserveIdentifier}{\code{signature(x = "D1Client", id = "character")}: ... }
    \item{setMNodeId}{\code{signature(x = "D1Client", id = "character")}: ... }
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("D1Client")
}
\keyword{classes}

\name{DataPackage}
\alias{DataPackage}
\title{
    An object representing a data package in DataONE, which consists of a
    series of data objects, their science metadata, and system metadata
    describing them.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DataPackage(identifier, sysmeta1, scimeta1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{identifier}{
%%     ~~Describe \code{identifier} here~~
}
  \item{sysmeta1}{
%%     ~~Describe \code{sysmeta1} here~~
}
  \item{scimeta1}{
%%     ~~Describe \code{scimeta1} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (identifier, sysmeta1, scimeta1, ...) 
{
    standardGeneric("DataPackage")
  }, generic = structure("DataPackage", package = "dataone"), package = "dataone", group = list(), valueClass = character(0), signature = c("identifier", 
"sysmeta1", "scimeta1"), default = `NULL`, skeleton = function (identifier, 
    sysmeta1, scimeta1, ...) 
stop("invalid call in method dispatch to \"DataPackage\" (no default method)", 
    domain = NA)(identifier, sysmeta1, scimeta1, ...), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

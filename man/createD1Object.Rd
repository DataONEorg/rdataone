\name{createD1Object}
\alias{createD1Object}
\title{
  Create a D1Object by calling the DataONE create() service to upload an object
  and its associated system metadata to a Member Node.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
createD1Object(x, identifier, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{identifier}{
%%     ~~Describe \code{identifier} here~~
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
structure(function (x, identifier, ...) 
{
    standardGeneric("createD1Object")
  }, generic = structure("createD1Object", package = "dataone"), package = "dataone", group = list(), valueClass = character(0), signature = c("x", 
"identifier"), default = `NULL`, skeleton = function (x, identifier, 
    ...) 
stop("invalid call in method dispatch to \"createD1Object\" (no default method)", 
    domain = NA)(x, identifier, ...), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}
\keyword{manip}

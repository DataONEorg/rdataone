\name{createD1Object}
\alias{createD1Object}
\title{
  createD1Object
}
\description{
  Create a D1Object by calling the DataONE create() service to upload an object
  and its associated system metadata to a Member Node.
}
\usage{
createD1Object(x, d1Object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  The D1Client that will do the creating
}
  \item{d1Object}{
  The D1Object instance to be submitted to DataONE for creation.  
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
  The D1Object data along with generated system metadata will be submitted to the
  Member Node associated with the D1Client, using the current credentials to 
  populate the submitter and rightsholder fields.
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
  \code{
  	\link{"http://dev-testing.dataone.org:8080/hudson/job/d1_libclient_java/ws/d1_libclient_java/target/site/apidocs/org/dataone/client/D1Client.html#create(org.dataone.service.types.v1.Session,%20org.dataone.client.D1Object)"
       }
    }
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
  # (not executable!!)
  library(dataone)
  cli <- D1Client("STAGING","urn:node:memberNodeFoo")
  
  # a fictitious function that assembles a D1Object
  d1object <- makeANewObject()
  
  createD1Object(cli, d1object)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}
\keyword{manip}

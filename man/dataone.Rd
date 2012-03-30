\name{dataone}
\alias{dataone-package}
\title{Package to access data and metadata in the DataONE repository federation.}
\description{
  This package provides facilities to manage data and data within the DataONE
  federation of data repositories.  DataONE defines four tiers of standard
  services that can be implemented by participating repositories, called
  Member Nodes.  These services includes the ability to search and discover data
  and metadata objects across the whole federation, retrieve objects from particular
  Member Nodes, and write results and metadata to Member Nodes.  The latter functions
  require user authentication to function properly.  The dataone R package provides
  a set of convenience classes and functions for accessing these DataONE services.
}
\usage{
  dataone()
}
\details{
  Details are given in the package vignette.
}
\value{
  The string "Hello Java World!" is returned.
}
\author{Matthew Jones}
\examples{
  dataone()
}
\keyword{manip}

\name{tableDescriber.registry}
\alias{tableDescriber.registry}
\docType{data}
\title{
Registry of D1 formatIds where TableDescriber subclasses register
the metadata formats they can handle.
}
\description{
Classes that inherit from TableDescriber should register themselves to this
data structure when loaded, so it may be used by the \code{asDataFrame} methods.
}
%%\usage{data(tableDescriber.registry)}
\format{
  The format is:
List of 4
 $ eml://ecoinformatics.org/eml-2.1.1: chr "EMLParser"
 $ eml://ecoinformatics.org/eml-2.1.0: chr "EMLParser"
 $ eml://ecoinformatics.org/eml-2.0.1: chr "EMLParser"
 $ eml://ecoinformatics.org/eml-2.0.0: chr "EMLParser"
}
\author{
Matthew Jones and Rob Nahf
}
\examples{
data(tableDescriber.registry)
}
\keyword{datasets}
\keyword{dataone}

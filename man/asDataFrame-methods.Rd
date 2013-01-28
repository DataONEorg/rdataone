\docType{methods}
\name{asDataFrame}
\alias{asDataFrame}
\alias{asDataFrame,DataPackage,character-method}
\alias{asDataFrame,D1Object,D1Object-method}
\alias{asDataFrame,D1Object,DataTableDescriber-method}
\alias{asDataFrame,D1Object,ANY-method}
\title{Build a DataFrame From a Data Object}
\usage{
    asDataFrame(dataPackage, data.identifier, ...)
    asDataFrame(data.d1Object, dataTableDescriber, ...)
    asDataFrame(data.d1Object, metadata.d1Object, ...)
    asDataFrame(data.d1Object, ...)
}
\arguments{
  \item{dataPackage}{a DataPackage containing the data object}
  
  \item{data.d1Object}{the D1Object containing the data to 
  represent as a dataFrame}
  
  \item{metadata.d1Object}{the D1Object containing the metadata 
  that describes data object, from which to get parsing instructions}
  
  \item{dataTableDescriber}{a DataTableDescriber (or subclass thereof)
  that will provide parsing istructions for the data}
  
  \item{...}{parameters pass through to read.csv}
}

\description{
  The generic asDataFrame function returns the content of a
  DataONE object as a DataFrame, assuming that the object
  represents scientific data and can be coerced into
  tabular form.  Several implementations of this method
  exist to accomplish this.
}
\details{
  In many cases, a science metadata object includes in its
  description how the scientific data is stored, and this
  information should be used when converting from tabular
  form to a dataFrame.  The 2-argument implementations for
  asDataFrame exist to allow this to happen.

  Most users will use the implementation that takes the
  DataPackage and the member identifier of the data object
  to read into a dataFrame.  In this case, the the
  dataPackage isolates the appropriate science metadata
  object, finds the appropriate DataTableDescriber for it,
  and passes it to the single-argument
  \code{asDataFrame(D1Object,...)} method.

  The other double-argument methods are progressively more
  direct, and are most useful when there is not a
  DataPackage that can associate the metadata to the data.

  The DataTableDescriber class is abstract, with
  data-format-specific subclasses providing the specific
  logic for parsing that particular data format (for
  example EMLParser for reading and parsing EML documents).
  If there is no metadata parser for the metadata that
  describes the object, you will have to determine the
  correct parameters for read.csv manually.
}
\value{
  a dataFrame representation of the object's data
}
\note{
  As of Feb 2013, there is only one DataTableDescriber subclass
  for parsing metadata.  Called EMLParser, it can handle EML version
  2.0.0 - 2.1.1 formatted metadata files. Other parsers should become 
  available as separate packages that can be loaded as needed.
}
\examples{
\dontrun{
df <- asDataFrame(data.package, dataMember.id)

df <- asDataFrame(data.object, its.metadata)

table.describer <- EMLParser(its.metadata)
df <- asDataFrame(data.object, table.describer)

df <- asDataFrame(data.object, sep="\\t", ...)

df <- asDataFrame(data.object)

  }
}
\author{
  rnahf
}
\seealso{
  \code{\link{read.csv}}
}


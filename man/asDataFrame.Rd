\name{asDataFrame}
\alias{asDataFrame}
\title{
    Convert the D1Object's data into a data frame.
}
\description{
  The 'asDataFrame' methods deserialize the tabular data of a D1Object into a dataFrame,
  taking a variety of parameters to support a wide variety of situations.  Currently
  works for character delimited text formats.  Unsupported formats may throw exceptions.  
}
\usage{
  asDataFrame(x, reference)
  asDataFrame(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    either the D1Object to be written or the DataPackage containing the D1Object
    to be written.
	
  }
  \item{reference}{
    character, D1Object, DataTableDescriber. If 'x' is a DataPackage, 'reference' should
    be the identifier of the package member that holds the data.  Otherwise ('x' is a D1Object),
    can be either a D1Object representing the metadata that documents how the data is 
    physically represented, or a DataTableDescriber that directly provides deserialization
    instructions for that object.
  }
  \item{\dots}{
    The parameters that get passed to read.table
  }
}
\details{
  In it's most simple form, asDataFrame(x,...) converts the data bytes of the
  D1Object into text, and feeds that text into read.csv.  So with no other parameters
  a true csv file (comma field separator) would successfully be converted into
  a dataFrame.
  
  This doesn't work for all data sets, so any of the named parameters of 'read.table'
  can be added, 'sep="\t"' for example for tab delimited data.
  
  Manually figuring out what the correct parameters should be is a time-consuming
  process, especially if working with someone else's data.  In DataONE, the metadata
  of a data object often contains this information, and we'd like to be able to
  take advantage of this whenever possible.  
  
  The abstract DataTableDescriber class was designed to facilitate this.  It defines
  the methods that will be called to set these named parameters correctly, as well
  as a mechanism to find the available subclasses to the R session.  Accordingly, we
  provide methods that take the DataTableDescriber directly or the metadata D1Object
  to more automatically build the dataFrame.
  
  Finally, a DataPackage contains the information that associates a data D1Object
  with its "documenting" metadata D1Object, so we provide a method to simply provide
  the identifier of the data D1Object to convert it to dataFrame.  The method does 
  the work of finding the appropriate metadata object.
  
  When passing in the metadata object, or use the DataPackage, you are relying
  on the metadata-format-specific DataTableDescriber subclasses to be loaded into
  your R session, and not all formats are yet supported.
}
\value{
  a dataFrame representation of the data
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Rob Nahf
}
\note{
  The same cautions about memory usage as 'read.table' apply
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
  read.csv / read.table
}
\examples{


## a trivial working example for the manual workflow

## preparing some data for the examples 
## trees is a built-in dataset
d1.client <- D1Client("SANDBOX")
trees.csv <- convert.csv(trees)

## now the trees data is serialized into a D1Object
trees.d1o <- new("D1Object","exampleId", trees.csv, "text/csv", "node:urn:foo")

## now converted the data object known to be a standard csv file 
df <- asDataFrame(trees.d1o)


## other non-working examples
## (these depend on the presence of the proper DataTableDescriber")
#####################################################################

## providing a different field separator
df <- asDataFrame(data.object, sep="\t", ...)

## using a DataTableDesciber to provide instructions
table.describer <- EMLParser(its.metadata)
df <- asDataFrame(data.object, table.describer)

## using the metadata identifier, and letting the method
## find the correct DataTableDescriber based on the formatId of the metadata
df <- asDataFrame(data.object, its.metadata)

## finally, using the resourceMap to find the metadata object that describes
## the data object.
df <- asDataFrame(data.package, dataMember.id)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

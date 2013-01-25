\name{encodeUrlQuery}
\alias{encodeUrlQuery}
\title{Encode the Input for a URL Query Segment}
\usage{
  encodeUrlQuery(querySegment)
}
\arguments{
  \item{querySegment}{: a string to encode}
}
\value{
  the encoded form of the input
}
\description{
  Encodes the characters of the input so they are not
  interpretted as reserved characters in url strings.  Will
  also encode non-ASCII unicode characters.
}
\examples{
fullyEncodedQuery <- paste0("q=id:",encodeUrlQuery(encodeSolr("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5")))
}
\author{
  rnahf
}


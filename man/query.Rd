% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/D1Node.R
\docType{methods}
\name{query}
\alias{query}
\alias{query,D1Node-method}
\title{Search DataONE for data and metadata objects}
\usage{
query(d1node, ...)

\S4method{query}{D1Node}(d1node, solrQuery = as.character(NA),
  encode = TRUE, as = "list", parse = TRUE,
  searchTerms = as.character(NA), ...)
}
\arguments{
\item{d1node}{The CNode or MNode instance to send the query to.}

\item{...}{(Not yet used.)}

\item{solrQuery}{The query parameters to be searched, either as a string or as list with named attributes.}

\item{encode}{A boolean, if true then the entire query string is URLencoded if it is a character, or each parameter value if a list.}

\item{as}{The return type. Possible values: "json", "xml", "list" or "data.frame" with "list" as the default.}

\item{parse}{A boolean value. If TRUE, then the result is parsed and converted to R data types. If FALSE, text values are returned.}

\item{searchTerms}{A list of name / value pairs. Either \code{'searchTerms'} or \code{'solrQuery'} must be specified.}
}
\value{
search results
}
\description{
Use SOLR syntax to search the DataONE federation of data repositories for matching data.
}
\details{
Several different return types can be specified with the \code{"as"} parameter: "json", xml", "list", "data.frame".
If "xml" is specified and \code{'parsed=TRUE'} then the query result is returned as an R XMLInternalDocument. If \code{'parsed'} is
false then a character variable with the XML string is returned. Specify 'list' to have 
the result parseed to an R list, with each list element containing one Solr result as a list of values, for example.
As an alternative to specifying the Solr query terms using the \code{'solrquery'} parameter, the \code{'searchTerms'} parameter
can be specified. This parameter is a list with query field / value pairs, i.e. searchTerms=list(abstract=kelp, attribute=biomass).
The query fields can be listed for a DataONE node using \code{\link{getQueryEngineDescription}} 
\code{'result[[1]]$id'} would be the DataONE identifier value of the first result (if the query parameters specified that
the id field shoudl be returned from the query). If \code{'json'} is specified, then the Solr response writer argument
\code{'&wt=json'} must be included in the \code{'solQuery'} parameter. Currently for a json return type the \code{'parse'} parameter
is ignored and unparsed text will always be returned.
Any lucene reserved characters in query parameters must be escaped with backslash, for example, 
\code{'queryParams <- "q=id:doi\\:10.6073/AA/knb-lter-and.4341.13"'}. Notice that the colon after
\code{'q=id'} is not escaped, as this is needed by Solr to parse the query.
If solrQuery is a list, 
it is expected to have field names as attributes and search values as the values in the list.
}
\examples{
\dontrun{
library(dataone)
cn <- CNode("PROD")
queryParams <- list(q="id:doi*", rows="5", 
    fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", 
    fl="title,id,abstract,size,dateUploaded,attributeName")
# Return result as a list.
result <- query(cn, queryParams, as="list")

# Query and return the result as a data.frame of character values.
queryParams <- list(q="id:doi*", rows="3", 
    fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", 
    fl="title,id,abstract,size,dateUploaded,attributeName")
result <- query(cn, queryParams, as="data.frame", parse=FALSE)

# Return the result as JSON
queryParams <- "q=id:doi*&rows=2&wt=json"
result <- query(cn, queryParams, as="json")

# The following query shows how to embed quotes
cn <- CNode("SANDBOX2")
queryParamList <- list(q="(attribute:lake) and (attribute:\\"Percent Nitrogen\\")", rows="1000",
                       fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
result <- query(cn, queryParamList, as="data.frame")

# The following query uses the searchTerms parameter
cn <- CNode()
mn <- getMNode(cn, "urn:node:KNB")
mySearchTerms <- list(abstract="kelp", attribute="biomass")
result <- query(mn, searchTerms=mySearchTerms, as="data.frame")
}
}

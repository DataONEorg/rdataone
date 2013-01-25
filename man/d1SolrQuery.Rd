\name{d1SolrQuery}
\alias{d1SolrQuery}
\title{A method to query the DataONE solr endpoint of the Coordinating Node.
It expects any lucene reserved characters to already be escaped with backslash.}
\arguments{
  \item{x}{the D1Client (environment) being queried}

  \item{solrQuery}{list}

  \item{x}{D1Client: representing the DataONE environment
  being queried}

  \item{solrQuery}{character: a fully encoded query string}
}
\value{
  the solr response (XML)

  the solr response (XML)
}
\description{
  A method to query the DataONE solr endpoint of the
  Coordinating Node. It expects any lucene reserved
  characters to already be escaped with backslash.

  A method to query the DataONE solr endpoint of the
  Coordinating Node. It expects a fully encoded character
  string as input (with lucene-reserved characters
  backslash escaped and url-reserved characters %-encoded).
}
\note{
  users should not provide the leading '?' to the query
}
\examples{
\dontrun{ d1SolrQuery(client,list(q="+species population diversity", fl="identifier")) }
\dontrun{ d1SolrQuery(client,"q=\%2Bspecies\%20population\%20diversity\%26fl=identifier" }
}
\author{
  rnahf

  rnahf
}


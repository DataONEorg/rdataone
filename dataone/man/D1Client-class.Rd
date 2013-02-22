\name{D1Client-class}
\Rdversion{1.1}
\docType{class}
\alias{D1Client}
\alias{convert.csv}
\alias{createD1Object}
\alias{createDataPackage}
\alias{d1IdentifierSearch}
\alias{d1SolrQuery}
\alias{d1SolrQuery}
\alias{encodeSolr}
\alias{encodeUrlQuery}
\alias{encodeUrlPath}
\alias{getCN}
\alias{getD1Object}
\alias{getEndpoint}
\alias{getMN}
\alias{getMNodeId}
\alias{getPackage}
\alias{listMemberNodes}
\alias{reserveIdentifier}
\alias{setMNodeId}
\alias{D1Client-class}
\alias{convert.csv,D1Client-method}
\alias{createD1Object,D1Client,D1Object-method}
\alias{createDataPackage,D1Client,DataPackage-method}
\alias{d1IdentifierSearch,D1Client,character-method}
\alias{d1SolrQuery,D1Client,character-method}
\alias{d1SolrQuery,D1Client,list-method}
\alias{encodeSolr,D1Client,character-method}
\alias{encodeUrlQuery,D1Client,character-method}
\alias{encodeUrlPath,D1Client,character-method}
\alias{getCN,D1Client-method}
\alias{getD1Object,D1Client-method}
\alias{getEndpoint,D1Client-method}
\alias{getMN,D1Client,ANY-method}
\alias{getMN,D1Client,character-method}
\alias{getMNodeId,D1Client-method}
\alias{getPackage,D1Client,character-method}
\alias{listMemberNodes,D1Client-method}
\alias{reserveIdentifier,D1Client,character-method}
\alias{setMNodeId,D1Client,character-method}

\title{Class \code{"D1Client"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
A D1Client object represents the high-level interface to the DataONE system,
and its methods mediate most all interaction with the system.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("D1Client", ...)} or 
using one of the constructor methods:  \code{\link{D1Client}}
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{endpoint}:}{Object of class \code{"character"} ~~ }
    \item{\code{mn.nodeid}:}{Object of class \code{"character"} ~~ }
    \item{\code{client}:}{Object of class \code{"jobjRef"} ~~ }
    \item{\code{session}:}{Object of class \code{"jobjRef"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{convert.csv}{\code{signature(x = "D1Client")}: 
    returns the dataframe serialized as a .csv 
    }
    \item{createD1Object}{\code{signature(x = "D1Client", d1Object = "D1Object")}: 
    submits the objects of the Data Package for creation in the DataONE System.
    }
    \item{createDataPackage}{\code{signature(x = "D1Client", dataPackage = "DataPackage")}: 
    Creates the D1Objects contained in the DataPackage by calling the createD1Object() 
    on each of the members, as well as assembling the resourceMap object from the
    recorded relationships, and calling create() on it as well. Any objects in 
    the data map that have a dataUploaded value are assumed to be pre-existing in 
    the system, and skipped. 
    }
    \item{d1IdentifierSearch}{\code{signature(x = "D1Client", solrQuery = "character")}: 
     query the DataONE solr endpoint of the Coordinating Node, and return a character 
     vector of identifiers.  It expects a fully encoded character string as input 
     (with lucene-reserved characters backslash escaped and url-reserved characters 
     percent-encoded). 
    }
    \item{d1SolrQuery}{\code{signature(x = "D1Client", solrQuery = "character")}: 
    query the DataONE solr endpoint of the Coordinating Node. It expects 
    a fully encoded character string as input (with lucene-reserved characters
    backslash escaped and url-reserved characters \%-encoded). 
    }
    \item{d1SolrQuery}{\code{signature(x = "D1Client", solrQuery = "list")}: 
    query the DataONE solr endpoint of the Coordinating Node. the solrQuery list
    expects named parameters corresponding to SOLR url query constructs.  Values of
    the list are expected to backslash escape any lucene reserved characters.
    }
    \item{encodeSolr}{\code{signature(x = "D1Client", segment = "character")}: 
    treating all special characters and spaces as literals, backslash escape 
    special characters, and surround with double-quotes if necessary
    }
    \item{encodeUrlQuery}{\code{signature(x = "D1Client", querySegment = "character")}: 
     Encodes the characters of the input so they are not interpreted as reserved 
     characters in url strings.  Will also encode non-ASCII unicode characters.
    }
    \item{encodeUrlQuery}{\code{signature(x = "D1Client", pathSegment = "character")}: 
     Encodes the characters of the input so they are not interpreted as reserved 
     characters in url strings.  Will also encode non-ASCII unicode characters.
    }
    
    
    \item{getCN}{\code{signature(x = "D1Client")}: 
    ... 
    }
    \item{getD1Object}{\code{signature(x = "D1Client")}: 
    retrieve an object as a D1Object from the DataONE system by its identifier 
    }
    \item{getEndpoint}{\code{signature(x = "D1Client")}: 
    Get the URI endpoint of the CN service which D1Client is using.
     This value is determined from the env parameter 
    }
    \item{getMN}{\code{signature(x = "D1Client", nodeid = "ANY")}: 
    ... 
    }
    \item{getMN}{\code{signature(x = "D1Client", nodeid = "character")}: 
    ...
    }
    \item{getMNodeId}{\code{signature(x = "D1Client")}: 
    returns the identifier for the default Member Node 
    }
    \item{getPackage}{\code{signature(x = "D1Client", identifier = "character")}: 
    retrieve a DataPackage from the DataONE system by its identifier, including
    all of it's members.   
    }
    \item{listMemberNodes}{\code{signature(x = "D1Client")}: 
    list the nodes registered to the DataONE environment 
    }
    \item{reserveIdentifier}{\code{signature(x = "D1Client", id = "character")}: 
    reserve an identifier on DataONE in preparation for building and submitting
    a new object. 
    }
    \item{setMNodeId}{\code{signature(x = "D1Client", id = "character")}: 
    sets the default Member Node, to which future submissions will be directed. Note,
    D1Objects already built will be directed at the previous Member Node when createD1Object
    is called, since their systemMetadata specifies the old value.
    }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
 See d1_libclient_java documentation D1Client.create()
  \url{"http://dev-testing.dataone.org:8080/hudson/job/d1_libclient_java/ws/d1_libclient_java/target/site/apidocs/org/dataone/client/D1Client.html#create"}

}
\author{
Matthew Jones and Rob Nahf
}
\note{
%%  ~~further notes~~
users should not provide the leading '?' to any query methods

  The DataPackage describes the collection of data object
  and their associated metadata object, with the
  relationships and members serialized into a document
  stored under, and retrievable with, the packageId as it's
  own distinct object.

  Members are created serially, and most errors in creating
  one object will interrupt the create process for the
  whole, resulting in some members will getting created,
  and the remainder not.
}

\examples{
showClass("D1Client")
\dontrun{ 
    encodeSolr(client, "this & that") 
    fullyEncodedPath <- paste0("cn/v1/object/",encodeUrlPath("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5"))
    fullyEncodedQuery <- paste0("q=id:",encodeUrlQuery(encodeSolr("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5")))

    d1IdentifierSearch(client,"q=\%2Bspecies\%20population\%20diversity")
    d1SolrQuery(client,list(q="+species population diversity", fl="identifier")) 
    d1SolrQuery(client,"q=\%2Bspecies\%20population\%20diversity\%26fl=identifier")
}

}
\keyword{classes}

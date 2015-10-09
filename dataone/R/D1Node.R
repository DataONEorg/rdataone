#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2013
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

#' @include auth_request.R
## A class representing a Member Node repository, which can expose and store data
## @slot identifier The node identifier of the MN
## @slot name The node name
## @slot description The node description
## @slot baseURL The registered baseURL for the node, which does not include the version string
## @slot subject The Distinguished Name of this node, used for authentication
## @slot contactSubject The Distinguished Name of contact person for this node
## @slot replicate  a boolean flag indicating whether the node accepts replicas
## @slot type the node type, either 'mn' or 'cn'
## @slot state an indication of whether the node is accessible, either 'up' or 'down'
## @slot serviceUrls a data.frame that contains DataONE service Urls
## @author jones
## @export
setClass("D1Node",
         slots = c(	identifier = "character",
					name = "character",
					description = "character",
					baseURL = "character",
					#services,
					#synchronization,
					#nodeReplicationPolicy,
					#ping,
					subject = "character",
					contactSubject = "character",
					replicate = "character",
					type = "character",
					state = "character",
          serviceUrls = "data.frame"
					)
)

#########################
## Node constructors
#########################

## @param baseurl The node URL with which this node is registered in DataONE
## @param ... (not yet used)
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("D1Node", function(xml, ...) {
  standardGeneric("D1Node")
})

setMethod("initialize", "D1Node",function(.Object) {
    info <- sessionInfo()
    # Force loading of packages now, to get package info
   return(.Object)
})

## Construct a Node, using a passed in node url
## @param baseurl The node url with which this node is registered in DataONE
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setMethod("D1Node", signature("XMLInternalElementNode"), function(xml) {

  ## create new Node object
	node <- new("D1Node")
	newnode <- parseCapabilities(node, xml)
	return(newnode)
})

##########################
## Methods
##########################

# The MN and CN APIs have several services with the same name, i.e. "get', 'getSystemMetadata', 'describe', etc.,
# so MNode.R and CNode.R have several methods that also share the same name. The generic functions for these 
# methods are defined here in the parent class, so that the generic is defined for all child classes (MNode.R, CNode.R), where
# the corresponding methods are defined.

#' Archive an object on a Member Node or Coordinating Node, which hides it from casual searches.
#' @description This method provides the ability to archive a data or metadata object on the Member Node
#' provided in the \code{'mnode'} parameter.  Archiving removes the object from DataONE search functions,
#' thereby making it more difficult to find without completely removing the object.  Archive is intended
#' for objects that should not be used by current researchers, but for which there is a desire to maintain
#' a historical record, such as when journal articles might cite the object.  Users can still obtain the
#' contents of archived objects if they have the identifier, but will not discover it through searches.
#' @details This operation requires an X.509 certificate to be present in the default location of the file 
#' system. This certificate provides authentication credentials from 
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' Also, administrator priviledge is required to run archive() on a DataONE Coordinating Node.
#' @param node The MNode or CNode instance on which the object will be created
#' @param pid The identifier of the object to be created
#' @param ... (Not yet used)
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.archive}
#' @export
setGeneric("archive", function(node, pid, ...) {
  standardGeneric("archive")
})

#' @describeIn archive
#' @param quiet A logical value - if TRUE (the default) then informational messages are not printed.
#' @return The pid that was archived if successful, otherwise NULL
setMethod("archive", signature("D1Node", "character"), function(node, pid, quiet=TRUE) {
    url <- paste(node@endpoint, "archive", URLencode(pid, reserve=TRUE), sep="/")
    response <- auth_put(url)
    if(response$status != "200") {
        if(!quiet) {
            message(sprintf("Error archiving %s\n", pid))
        }
        return(NULL)
    } else {
        # Comment out body handling because httr::PUT is not returning a response body at all
        #resultText <- content(response, as="text")
        #doc <- xmlInternalTreeParse(resultText)
        # XML doc is similiar to: <d1:identifier xmlns:d1="http://ns.dataone.org/service/types/v1">WedSep91341002015-ub14</d1:identifier>
        #nodes <- getNodeSet(doc, "/d1:identifier")
        #id <- xmlValue(nodes[[1]])
        # Return the identifier as a character value
        #return(id)
        return(pid)
    }
})

#' Get the bytes associated with an object on this Node.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The Node instance from which the pid will be downloaded
#' @param pid The identifier of the object to be downloaded
#' @return the bytes of the object
#' @export
setGeneric("get", function(node, pid, ...) {
  standardGeneric("get")
})

#' Returns the checksum for a pid 
#' @param node The CNode or MNode instance from which the checksum will be retrieved
#' @param pid The object identifier to be downloaded
#' @return checksum The comuted hash of the pid object
#' @export
setGeneric("getChecksum", function(node, pid, ...) {
  standardGeneric("getChecksum")
})

#' Retrieve a description of a query engine from a coordinating node or member node
#' @param node The CNode or MNode to query
#' @param queryEngine The query engine name to get a description for.
#' @return list The query engine description
#' @export
setGeneric("getQueryEngineDescription", function(node, queryEngineName) {
  standardGeneric("getQueryEngineDescription")
})

#' Query a node for the list of query engines available on the node
#' @param node The CNode or MNode instance to list the query engines for.
#' @return list Objects that met the search criteria
#' @export
#' @examples
#' \dontrun{ 
#' cn <- CNode("PROD")
#' engineDesc <- getQueryEngineDescription(cn, "solr")
#' cat(sprintf("Query engine version: %s\n", engineDesc[1]$queryEngineVersion))
#' cat(sprintf("Query engine name: %s\n", engineDesc[2]$name))
#' for (i in 5:length(engineDesc)) {
#'   cat(sprintf("query field: %s : %s\n", engineDesc[i]$queryField$name, engineDesc[i]$queryField$description))
#' }
#' }
#' @describeIn CNode
setMethod("getQueryEngineDescription", signature("D1Node", "character"), function(node, queryEngineName) {
  
  url <- paste(node@endpoint, "query", queryEngineName, sep="/")
  # Send the request
  response<-GET(url)
  if (is.raw(response$content)) {
    tmpres <- content(response, as="raw")
    resultText <- rawToChar(tmpres)
  } else {
    resultText <- content(response, as="text")
  }
  
  # Parse the returned XML into a list
  queryEngineDescription <-(xmlToList(xmlParse(resultText)))
  
  return(queryEngineDescription)
})

#' Get the metadata describing system properties associated with an object on this Node.
#' @description The SystemMetadata includes information about the identity, type, access control, and other system
#' level details about the object.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The Node instance from which the SystemMetadata will be downloaded
#' @param pid The identifier of the object
#' @return SystemMetadata for the object
#' @import datapackage
#' @export
setGeneric("getSystemMetadata", function(node, pid, ...) {
  standardGeneric("getSystemMetadata")
})

#' This method provides a lighter weight mechanism than getSystemMetadata() for a client to
#' determine basic properties of the referenced object.
#' @param mnode The Node instance from which the identifier will be generated
#' @param pid Identifier for the object in question. May be either a PID or a SID. Transmitted as
#' part of the URL path and must be escaped accordingly.
#' @return A list of header elements
#' @export
#' @author Scott Chamberlain
setGeneric("describe", function(node, pid, ...) {
  standardGeneric("describe")
})

#' Retrieve the list of objects that match the search parameters
#' @param node The Node instance from which the SystemMetadata will be downloaded
#' @return list Objects that met the search criteria
#' @export
setGeneric("listObjects", function(node, ...) {
  standardGeneric("listObjects")
})

#' Retrieve the list of objects present on the MN that match the calling parameters. 
#' @details The list of objects that is returned is paged according to the \code{'start'} and
#' \code{'count'} values, so that large result sets can be returned over multiple calls.
#' @param node The MNode or CNode instance from which the checksum will be retrieved
#' @param fromDate Entries with a modified date greater than \code{'fromDate'} will be returned.
#' This value must be specified in ISO 8601 format, i.e. "YYYY-MM-DDTHH:MM:SS.mmm+00:00"
#' @param toDate Entries with a modified date less than \code{'toDate'} will be returned.
#' This value must be specified in ISO 8601 format, i.e. "YYYY-MM-DDTHH:MM:SS.mmm+00:00"
#' @param formatId The format to match, for example "eml://ecoinformatics.org/eml-2.1.1"
#' @param replicaStatus A logical value that determines if replica (object not on it's origin node) should be returned. Default is TRUE.
#' @param start An integer that specifies the first element of the result set that will be returned
#' @param count An integer that specifies how many results will be returned
#' @return list Objects that met the search criteria
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.listObjects}
#' @import parsedate
#' @export
#' @describeIn MNode
setMethod("listObjects", signature("D1Node"), function(node, 
                                                      fromDate=as.character(NA), toDate=as.character(NA),
                                                      formatId=as.character(NA), replicaStatus=as.logical(TRUE), 
                                                      start=as.integer(0), count=as.integer(1000)) {
  
  # Build a parameter list with the specified arguments. Don't include parameters that
  # have not be specified and do not have default values, as each parameter included in the
  # list will be sent in the http query parameters.
  params <- list()
  if (!is.na(fromDate)) {
    chkDT <- parse_iso_8601(fromDate)
    if(is.na(chkDT)) stop(sprintf('Invalid parameter "fromDate=%s". Value must be in ISO 8601 format\n', fromDate))
    params <- c(params, fromDate=fromDate)
  }
  if (!is.na(toDate)) {
    chkDT <- parse_iso_8601(toDate)
    if(is.na(chkDT)) stop(sprintf('Invalid parameter "toDate=%s". Value must be in ISO 8601 format\n', toDate))
    params <- c(params, toDate=toDate)
  }
  if (!is.na(formatId)) {
    params <- c(params, formatId=URLencode(formatId))
  }
  params <- c(params, replicaStatus=as.character(replicaStatus))
  params <- c(params, start=as.character(start))
  params <- c(params, count=as.character(count))
  
  url <- paste(node@endpoint, "object", sep="/")
  # Send the request
  response<-GET(url, query=params)
  if (is.raw(response$content)) {
    tmpres <- content(response, as="raw")
    resultText <- rawToChar(tmpres)
  } else {
    resultText <- content(response, as="text")
  }
  
  # Parse the returned XML into a list
  objects<-(xmlToList(xmlParse(resultText)))
  return(objects)
})

#' List the query engines available for a DataONE member node or coordinating node
#' @param node The CNode or MNode to list the query engines for.
#' @return list The list of query engines.
#' @export
setGeneric("listQueryEngines", function(node, ...) {
  standardGeneric("listQueryEngines")
})

#' Query a node for the list of query engines available on the node
#' @param node The CNode or MNode instance to list the query engines for.
#' @return list Objects that met the search criteria
#' @family search engines
#' #For the \code{'logsolr'} search engine, \code{\link{logsolr}} and 
#' \url{http://jenkins-1.dataone.org/jenkins/job/API Documentation - trunk/ws/api-documentation/build/html/design/UsageStatistics.html}
#' @export
#' @describeIn CNode
setMethod("listQueryEngines", signature("D1Node"), function(node) {
  
  url <- paste(node@endpoint, "query", sep="/")
  # Send the request
  response<-GET(url)
  if (is.raw(response$content)) {
    tmpres <- content(response, as="raw")
    resultText <- rawToChar(tmpres)
  } else {
    resultText <- content(response, as="text")
  }
  
  # Parse the returned XML into a list
  resultList <-(xmlToList(xmlParse(resultText)))
  
  queryEngines <- list()
  # Reformat the list for easier consumption
  for (i in 1:length(resultList)) {
    queryEngines <- c(queryEngines, resultList[i]$queryEngine)
  }

  return(queryEngines)
})

## Construct a Node, using a passed in capabilities XML
## @param node The node to which capabilities should be applied.
## @param ... (not yet used)
## @returnType Node  
## @return the Node object with modified capabilities properties from the XML
## 
## @author jones
## @export
setGeneric("parseCapabilities", function(node, xml, ...) {
  standardGeneric("parseCapabilities")
})

## Construct a Node, using a passed in capabilities XML
## @param node The node to which capabilities should be applied.
## @param xml The XML capabilities representing the node to be created
## @returnType Node  
## @return the Node object with modified capabilities properties from the XML
## 
## @author jones
## @export
setMethod("parseCapabilities", signature("D1Node", "XMLInternalElementNode"), function(node, xml) {
  
  ## Parse the rest of the node information
  node@identifier <- xmlValue(xml[["identifier"]])
  node@name <- xmlValue(xml[["name"]])
  node@description <- xmlValue(xml[["description"]])
  node@baseURL <- xmlValue(xml[["baseURL"]])
  node@subject <- xmlValue(xml[["subject"]])
  node@contactSubject <- xmlValue(xml[["contactSubject"]])
  attrs <- xmlAttrs(xml)
  node@replicate <- attrs[["replicate"]]
  node@type <- attrs[["type"]]
  node@state <- attrs[["state"]]
  return(node)
})

#' Test if a node is online and accepting DataONE requests
#' @param node The CNode or MNode to check
#' @return logical A logical value set to TRUE if the node is up and FALSE if it is not
#' @export
setGeneric("ping", function(node) {
  standardGeneric("ping")
})

#' Test if a node is online and accepting DataONE requests
#' @param node The CNode or MNode to check.
#' @return list Objects that met the search criteria
#' @export
setMethod("ping", signature("D1Node"), function(node) {
  
  url <- paste(node@endpoint, "monitor/ping", sep="/")
  # Send the request
  response<-GET(url)

  if (response$status == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

## This function parses a DataONE service response message for errors, and extracts and
## prints error information.
## @param x The DataONE service response
##
## @author Scott Chamberlain
d1_errors <- function(x){
  headnames <- names(x$headers)
  tmp <- grep('dataone-exception-description', headnames, value = TRUE)
  exc_name <- x$headers$`dataone-exception-name`
  detailcode <- x$headers$`dataone-exception-detailcode`
  mssg <- sub('dataone-exception-description: ', '', tmp)
  cat(sprintf('Exception name: %s', exc_name), "\n")
  cat(sprintf('Exception detail code: %s', detailcode), "\n")
  cat(sprintf('Exception description: %s', mssg), "\n")
  #  list(exc_name=exc_name, detailcode=detailcode, message=mssg)
}

# Extract an error message from an http response. Http requests can fail
# for a variety of reasons, so getErrorDescription first tries to
# determine what type of response was sent. 
# The return types handled by this function are:
#   o An incorrect url is sent to DataONE and an error is returned by
#     the web server, not a specified DataOne service url. In this case,
#     a generic error message may be returned, e.g. status=404, URL not found
#   o A DataOne service was called, and retunred an error message. In this
#     case the DataONE response is parsed in an attemp to retrieve a
#     meaningfull error message.
# 
getErrorDescription <- function(response) {
  responseContent <- content(response, as="parsed")
  # DataONE services return XML
  if (class(responseContent)[1] == "XMLInternalDocument") {
     msgNode <- getNodeSet(responseContent, "/error/description")
     if (length(msgNode) > 0) {
       errorMsg <- xmlValue(msgNode[[1]])
     } else {
       # Don't know how to get error, so return generic error
       errorMsg <- http_status(response)$message
     }
  } else if (class(responseContent)[1] == "HTMLInternalDocument") {
    # To complex to try to get an error message from HTML, so
    # just get info from the response object. This will be a
    # generic message, so not as informative as specific msg from DataONE
    errorMsg <- http_status(response)$message
  }
  
  return(errorMsg)
}

#' Encode the input for Solr Queries
#' 
#' Treating all special characters and spaces as literals, backslash escape special
#' characters, and double-quote if necessary.
#' @param segment : a string to encode
#' @return the encoded form of the input
#' @examples encodeSolr("this & that")
#' @export
setGeneric("encodeSolr", function(segment, ... ) {
    standardGeneric("encodeSolr")
})

#' @export
setMethod("encodeSolr", signature(segment="character"), function(segment, ...) {
    inter <- gsub("([-+:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","\\\\\\1",segment, perl=TRUE) 
    if (grepl(" ",inter)) {
        return(paste0("\"",inter,"\""))
    }
    return(inter)
})

#' Search DataONE for data and metadata objects
#' @description Use SOLR syntax to search the DataONE federation of data repositories for matching data.
#' @details
#' Several different return types can be specified with the \code{"as"} parameter: "json", xml", "list", "data.frame".
#' If "xml" is specified and \code{'parsed=TRUE'} then the query result is returned as an R XMLInternalDocument. If \code{'parsed'} is
#' false then a character variable with the XML string is returned. Specify 'list' to have 
#' the result parseed to an R list, with each list element containing one Solr result as a list of values, for example.
#' \code{'result[[1]]$id'} would be the DataONE identifier value of the first result (if the query parameters specified that
#' the id field shoudl be returned from the query). If \code{'json'} is specified, then the Solr response writer argument
#' \code{'&wt=json'} must be included in the \code{'solQuery'} parameter. Currently for a json return type the \code{'parse'} parameter
#' is ignored and unparsed text will always be returned.
#' Any lucene reserved characters in query parameters must be escaped with backslash, for example, 
#' \code{'queryParams <- "q=id:doi\\:10.6073/AA/knb-lter-and.4341.13"'}. Notice that the colon after
#' \code{'q=id'} is not escaped, as this is needed by Solr to parse the query.
#' If solrQuery is a list, 
#' it is expected to have field names as attributes and search values as the values in the list.
#' @param d1node The coordinating node or member node object instance to query.
#' @param solrQuery The query parameters to be searched, either as a string or as list with named attributes.
#' @param encode A boolean, if true then the entire query string is URLencoded if it is a character, or each parameter value if a list.
#' @param as The return type. Possible values: "json", "xml", "list" or "data.frame" with "list" as the default.
#' @param parse A boolean value. If TRUE, then the result is parsed and converted to R data types. If FALSE, text values are returned.
#' @return search results
#' @import plyr
#' @examples
#' \dontrun{
#' cn <- CNode("PROD")
#' queryParams <- list(q="id:doi*", rows="5", fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", fl="title,id,abstract,size,dateUploaded,attributeName")
#' result <- query(cn, queryParams, as="list")
#' 
#' queryParams <- list(q="id:doi*", rows="3", fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", fl="title,id,abstract,size,dateUploaded,attributeName")
#' result <- query(cn, queryParams, as="data.frame", parse=FALSE)
#' 
#' queryParams <- "q=id:doi*&rows=2&wt=json"
#' result <- query(cn, queryParams, as="json")
#' }
#' @export
setGeneric("query", function(d1node, ...) {
  standardGeneric("query")
})

#' @export
setMethod("query", signature("D1Node"), function(d1node, solrQuery, encode=TRUE, as="list", parse=TRUE, ...) {
  
  returnTypes <- c("json", "xml", "list", "data.frame")
  if (!is.element(as, returnTypes)) {
    stop(sprintf("Invalid return type: \"%s\". Please specify one of \"%s\"", as, paste(returnTypes, collapse=",")))
  }
  # The CN API has a slightly different format for the solr query engine than the MN API,
  # so the appropriate URL is set in the CNode or MNode class.
  serviceUrl <- d1node@serviceUrls[d1node@serviceUrls$service=="query.solr", "Url"]
                        
  # The 'solrQuery' parameter can be specified as either a character string or a named list
  if (is(solrQuery, "list")) {
    encodedKVs <- character()
    for(key in attributes(solrQuery)$names) {
      if (encode) {
        kv <- paste0(key, "=", URLencode(solrQuery[[key]]))
      } else {
        kv <- paste0(key, "=", solrQuery[[key]])
      }
      encodedKVs[length(encodedKVs)+1] <- kv
    }
    queryParams <- paste(encodedKVs,collapse="&")
  } else {
    if (encode) {
      queryParams <- URLencode(solrQuery)
    } else {
      queryParams <- solrQuery
    }
  }
  
  queryUrl <- paste(serviceUrl, queryParams, sep="")

  # Send the query to the Node
  response <- auth_get(queryUrl)
  if(response$status != "200") {
    cat(sprintf("Error accessing %s: %s\n", queryUrl, getErrorDescription(response)))
    return(NULL)
  }
  
  # The CNs return the response content as binary regardless of Solr response writer specified or http request header
  # "Accept" type specified, so check the response and parse it to parsed XML, if necessary.
  if (is.raw(response$content)) {
    tmpres <- content(response, as="raw")
    resultText <- rawToChar(tmpres)
  } else {
    resultText <- content(response, as="text")
  }
  
  # Return result as unparsed XML text returned from Solr
  if (as == "xml") {
    if (parse) {
      # Return as XML tree structure
      res <- xmlInternalTreeParse(resultText, asText=TRUE)
    } else {
      res <- resultText
    }
  } else if (as == "json") {
    # if json return type, only unparsed text is supported
    res <- resultText
  } else if (as == "list") {
    # Return as a list, with data in each column returned as the appropriate R data type
    xmlDoc <- xmlInternalTreeParse(resultText, asText=TRUE)
    res <- parseSolrResult(xmlDoc, parse)
  } else if (as == "data.frame") {
    # Return as a data frame, all values represented as strings
    xmlDoc <- xmlInternalTreeParse(resultText, asText=TRUE)
    # First get a result as a list, then convert the list to a data frame
    res <- parseSolrResult(xmlDoc, parse)
    dfAll <- data.frame()
    if (length(res) > 0) {
      for (i in 1:length(res)) {
        df1 <- as.data.frame(res[[i]], stringsAsFactors=FALSE)
        # Have to use plyr:rbind.fill, as each row from the list could contain
        # a different number of field values, and hence data frame columns, and
        # each row of the data frame must have the same number of columns. Thanks H.W. for rbind.fill!
        dfAll <- rbind.fill(dfAll, df1)
      }
    }
    res <- dfAll
  }
  
  return(res)
})

#' Parse Solr output into an R list
#' @description Solr output that is specified with a writer type of XML \code{'&wt="xml"'}
#' @param result The Solr result to parse, in XML format
#' @return resultList The Solr result as an R list
#' @export
setGeneric("parseSolrResult", function(doc, ...) {
  standardGeneric("parseSolrResult")
})

#' @export
setMethod("parseSolrResult", signature("XMLInternalDocument"), function(doc, parse, ...) {
  resultList <- xpathApply(doc, "/response/result/doc", parseResultDoc, parse)
  return (resultList)
  
})

## Internal functions

# Parse a Solr result "<doc>" XML element inta an R list
parseResultDoc <- function(xNode, parse) {
  childNodes <- getNodeSet(xNode, "*")
  thisDocList <- list()
  for (child in childNodes) {
    thisDocList <- parseResultNode(child, thisDocList, parse)
  }
  return(thisDocList)
}

# Parse a Solr result field
parseResultNode <- function(xNode, resultList, parse) {
  nodeName <- xmlName(xNode)
  # parse a Solr result "arr" (arrary) into an R list
  if (nodeName == "arr") {
    childNodes <- getNodeSet(xNode, "*")
    # If we are not parseing Solr results to R types, then return this
    # list as a comma separated list of values
    if (parse) {
      resultList[[xmlGetAttr(xNode, "name")]] <- lapply(childNodes, parseSolrField, parse)
    } else {
      resultList[[xmlGetAttr(xNode, "name")]] <- paste(lapply(childNodes, parseSolrField, parse), collapse=",")
    }
    #xmlVals <- xpathApply(xNode, "*", parseResultNode, resultList=resultList)
  } else {
    # parse a Solr result atomic value into an R variable
    resultList[[xmlGetAttr(xNode, "name")]] <- parseSolrField(xNode, parse)
    # cat(sprintf("name: %s, value %d\n", valueType, nodeValue))
  }
  return(resultList)
}

# parse a Solr result field into an R variable
parseSolrField <- function(xNode, parse) {
  nodeName <- xmlName(xNode)
  if (parse) {
    if (nodeName == "arr") {
      warning(sprintf("Unable to process solr 'arr' field"))
      return(as.character(NULL))
    } else if (nodeName == "long" || nodeName == "float") {
      return(as.numeric(xmlValue(xNode)))
    } else if (nodeName == "str") {
      return(as.character(xmlValue(xNode)))
    } else if (nodeName == "bool") {
      return(as.logical(xmlValue(xNode)))
    } else if (nodeName == "int") {
      return(as.numeric(xmlValue(xNode)))
    } else if (nodeName == "date") {
      return(as.Date(xmlValue(xNode)))
    } else {
      warning(sprintf("Unhandled Solr field data type: %s\n", nodeName))
    }
  } else {
    return(xmlValue(xNode))
  }
}

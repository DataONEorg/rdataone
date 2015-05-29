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
          serviceUrls = "data.frame",
          userAgent = "character"
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
    library("RCurl")
    library("httr")
    .Object@userAgent <- sprintf("dataone/%s curl/%s Rcurl/%s httr/%s", 
                                 info$otherPkgs$dataone$Version, 
                                 curlVersion()$version, 
                                 info$otherPkgs$RCurl$Version, 
                                 info$otherPkgs$httr$Version)
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
  # TODO: add credentials if authenticated
  
  # Send the query to the Node
  response <- GET(queryUrl)
  if(response$status != "200") {
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
    for (i in 1:length(res)) {
      df1 <- as.data.frame(res[[i]], stringsAsFactors=FALSE)
      # Have to use plyr:rbind.fill, as each row from the list could contain
      # a different number of field values, and hence data frame columns, and
      # each row of the data frame must have the same number of columns. Thanks H.W. for rbind.fill!
      dfAll <- rbind.fill(dfAll, df1)
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

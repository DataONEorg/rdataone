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

#' @include D1Node.R
#' @include auth_request.R

#' A CNode represents a DataONE Coordinating Node and can be used to access its services.
#' @exportClass CNode
setClass("CNode", slots = c(endpoint = "character"), contains="D1Node")

#########################
## CNode constructors
#########################

#' Create a CNode object
#' @param env The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV')
#' @param ... (not yet used)
## @returnType CNode  
#' @return the CNode object representing the DataONE environment
## 
#' @author jones
#' @export
setGeneric("CNode", function(env, ...) {
  standardGeneric("CNode")
})

## 
## Construct a CNode, using default env ("PROD")
## @name CNode
## 
## @returnType CNode  
## @return the CNode object representing the DataONE environment
## 
## @author jones
## @docType methods
## @export
setMethod("CNode", , function() {
    result <- CNode("PROD")
    return(result)
})

## Pass in the environment to be used by this D1Client, plus the 
## id of the member node to be used for primary interactions such as creates
#' @export
setMethod("CNode", signature("character"), function(env) {

  ## Define the default CNs for each environment.
  PROD <- "https://cn.dataone.org/cn"
  STAGING <- "https://cn-stage.test.dataone.org/cn"
  STAGING2 <- "https://cn-stage-2.test.dataone.org/cn"
  SANDBOX <- "https://cn-sandbox.test.dataone.org/cn"
  SANDBOX2 <- "https://cn-sandbox-2.test.dataone.org/cn"
  DEV <- "https://cn-dev.test.dataone.org/cn"

  # By default, use production.  But also look in the environment.
  CN_URI <- PROD
  cn.url <- Sys.getenv("CN_URI")
  if(cn.url != "") {
    CN_URI <- cn.url
  } else {
    if (env == "DEV") CN_URI <- DEV
    if (env == "STAGING") CN_URI <- STAGING
    if (env == "STAGING2") CN_URI <- STAGING2
    if (env == "SANDBOX") CN_URI <- SANDBOX
    if (env == "SANDBOX2") CN_URI <- SANDBOX2
    if (env == "PROD") CN_URI <- PROD
  }

  ## create new D1Client object and insert uri endpoint
  result <- new("CNode")
  result@baseURL <- CN_URI
  result@endpoint <- paste(CN_URI, "v1", sep="/")
  # Set the service URL fragment for the solr query engine
  result@serviceUrls <- data.frame(service="query.solr", Url=paste(result@endpoint, "query", "solr", "select?", sep="/"), row.names = NULL, stringsAsFactors = FALSE)

  return(result)
})

##########################
## Methods
##########################

#' list formats
#' @description list of all object formats registered in the DataONE Object Format Vocabulary.
#' @param cnode a valid CNode object
#' @author hart
#' @import httr
#' @rdname listFormats-method
#' @seealso http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.listFormats
#' @return Returns a dataframe of all object formats registered in the DataONE Object Format Vocabulary.
#' @examples
#' \dontrun{
#' cn <- CNode()
#' listFormats(cn)
#' }
#' @export
setGeneric("listFormats", function(cnode, ...) {
  standardGeneric("listFormats")
})

#' list formats
#' @aliases listFormats
#' @describeIn CNode
#' @export
setMethod("listFormats", signature("CNode"), function(cnode) {
  url <- paste(cnode@endpoint,"formats",sep="/")
  out <- auth_get(url)
  out <- xmlToList(content(out,as="parsed"))
  ## Below could be done with plyr functionality, but I want to reduce
  ## dependencies in the package
  df <- data.frame(matrix(NA,ncol=3,nrow=(length(out)-1)))
  for(i in 1:(length(out)-1)){
    df[i,] <- c(unlist(out[[i]]))
  }
  colnames(df) <- c("ID","Name","Type")
  return(df)
})

#' Get information for a single DataONE object format
#' @param cnode A CNode object instance
#' @seealso http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.getFormat
#' @return Returns a dataframe of all object formats registered in the DataONE Object Format Vocabulary.
#' @examples
#' \dontrun{
#' cn <- CNode()
#' fmt <- getFormat(cn, "eml://ecoinformatics.org/eml-2.1.0")
#' cat(sprintf("format name: %s\n", fmt$name))
#' cat(sprintf("format type: %s\n", fmt$type))
#' cat(sprintf("format Id: %s\n", fmt$id))
#' }
#' @export
setGeneric("getFormat", function(cnode, ...) {
  standardGeneric("getFormat")
})

#' @aliases getFormat
#' @describeIn CNode
#' @export
setMethod("getFormat", signature("CNode"), function(cnode, formatId) {
  url <- paste(cnode@endpoint,"formats", URLencode(formatId), sep="/")
  response <- GET(url)
  
  if(response$status != "200") {
    return(NULL)
  }
  
  result <- xmlToList(content(response,as="parsed"))
  fmt <- list(name=result$formatName, type=result$formatType, id=result$formatId)
  return(fmt)
})

#' Get the checksum for the data object associated with the specified pid
#' @description A checksum is calculated for an object when it is uploaded to DataONE and
#' is submitted with the object's system metadata. The \code{'getChecksum'} method retrieves
#' the checksum from the specified coordinating node
#' @param node The CNode instance from which the checksum will be retrieved
#' @param pid The identifier of the object
#' @return character the checksum value, with the checksum algorithm as the attribute "algorithm"
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getChecksum}
#' @export
#' @describeIn CNode
setMethod("getChecksum", signature("CNode", "character"), function(node, pid) {
  url <- paste(node@endpoint, "checksum", pid, sep="/")
  response<-GET(url)
  if (is.raw(response$content)) {
    tmpres <- content(response, as="raw")
    resultText <- rawToChar(tmpres)
  } else {
    resultText <- content(response, as="text")
  }
  
  checksum<-(xmlToList(xmlParse(resultText)))
  returnVal <- checksum$text
  # Set an attribute of the algorithm used to calculate the checksum
  attr(returnVal, "algorithm") <- checksum$.attrs[['algorithm']]
  return(returnVal)
})

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.getLogRecords
# public Log getLogRecords(Date fromDate, Date toDate, Event event, String pidFilter, Integer start, Integer count) 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.listNodes
#' Get the list of nodes associated with a CN
#' @param cnode The coordinating node to query for its registered Member Nodes
#' @return the list of nodes in the DataONE CN environment
#' 
#' @export
setGeneric("listNodes", function(cnode, ...) {
    standardGeneric("listNodes")
})

#' @export
setMethod("listNodes", signature("CNode"), function(cnode) {
    url <- paste(cnode@endpoint, "node", sep="/")
    response <- auth_get(url, user_agent(cnode@userAgent))
    if(response$status != "200") {
        return(NULL)
    }
    
    xml <- content(response)
    node_identifiers <- sapply(getNodeSet(xml, "//identifier"), xmlValue)
    nodes <- getNodeSet(xml, "//node")
    nodelist <- sapply(nodes, D1Node)
    return(nodelist)
})


# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.reserveIdentifier
# public Identifier reserveIdentifier(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.generateIdentifier
# public Identifier generateIdentifier(String scheme, String fragment)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.hasReservation
# public boolean hasReservation(Subject subject, Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.setObsoletedBy
# public boolean setObsoletedBy(Identifier pid, Identifier obsoletedByPid, long serialVersion)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.delete
# public Identifier delete(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.archive
# public Identifier archive(Identifier pid)

#' Get the bytes associated with an object on this Coordinating Node.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The CNode instance from which the pid will be downloaded
#' @param pid The identifier of the object to be downloaded
#' @return the bytes of the object
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.get}
#' @export
#' @describeIn CNode
setMethod("get", signature("CNode", "character"), function(node, pid) {
    url <- paste(node@endpoint, "object", pid, sep="/")
    response <- auth_get(url)
    
    if(response$status != "200") {
        return(NULL)
    }
    
    return(content(response, as="raw"))
})

#' Get the metadata describing system properties associated with an object on a Coordinating Node.
#' @description The SystemMetadata includes information about the identity, type, access control, and other system
#' level details about the object.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The CNode instance from which the SystemMetadata will be downloaded
#' @param pid The identifier of the object
#' @return SystemMetadata for the object
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getSystemMetadata}
#' @import datapackage
#' @export
#' @describeIn CNode
setMethod("getSystemMetadata", signature("CNode", "character"), function(node, pid) {
    # TODO: need to properly URL-escape the PID
    url <- paste(node@endpoint, "meta", pid, sep="/")
    response <- auth_get(url)
    
    if(response$status != "200") {
        return(NULL)
    }
    
    # Convert the response into a SystemMetadata object
    sysmeta <- SystemMetadata(xmlRoot(content(response)))
    
    return(sysmeta)
})

#' This method provides a lighter weight mechanism than getSystemMetadata() for a client to
#' determine basic properties of the referenced object.
#' @seealso http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.describe
#' @param mnode The CNode instance from which the identifier will be generated
#' @param pid Identifier for the object in question. May be either a PID or a SID. Transmitted as
#' part of the URL path and must be escaped accordingly.
#' @return A list of header elements
#' @export
#' @describeIn CNode
#' @author Scott Chamberlain
setMethod("describe", signature("CNode", "character"), function(node, pid) {
  url <- file.path(node@endpoint, "object", pid)
  response <- HEAD(url)
  if(response$status != "200") {
    d1_errors(response)
  } else { return(unclass(response$headers)) }
})

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.resolve
# public ObjectLocationList resolve(Identifier pid)

#' Get a list of coordinating nodes holding a given pid
#' @description Returns a list of nodes (MNs or CNs) known to hold copies of the object identified by id.
#' @param cnode a valid CNode object
#' @param pid the id of the identified object
#' @docType methods
#' @author hart
#' @rdname resolve-method
#' @examples
#' \dontrun{
#' cn <- CNode("SANDBOX")
#' id <- "doi:10.5072/FK2/LTER/knb-lter-gce.100.15"
#' resolve(cn,id)
#' }
#' @export

setGeneric("resolve", function(cnode,pid) {
  standardGeneric("resolve")
})

#' @rdname resolve-method
#' @aliases resolve
#' @export
setMethod("resolve", signature("CNode" ,"character"), function(cnode,pid){
  url <- paste(cnode@endpoint,"resolve",pid,sep="/")
  config <- c(add_headers(Accept = "text/xml"), config(followlocation = 0L))
  out <- auth_get(url, nconfig=config)
  out <- xmlToList(content(out,as="parsed"))
  
  # Using a loop when plyr would work to reduce dependencies.
  df <- data.frame(matrix(NA,ncol=4,nrow=(length(out)-1)))
  for(i in 2:length(out)){
    df[(i-1),] <- c(unlist(out[[i]]))
  }
  colnames(df) <- c("identifier","baseURL","version","url")
  toret <- list(id = pid, data = df)
  return(toret)
})
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getChecksum
# public Checksum getChecksum(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.listObjects
# public ObjectList listObjects(Date fromDate, Date toDate, ObjectFormatIdentifier formatId, Boolean replicaStatus, Integer start, Integer count) 
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.search
# public ObjectList search(String queryType, String query)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getQueryEngineDescription
# public QueryEngineDescription getQueryEngineDescription(String queryEngine)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.listQueryEngines
# public QueryEngineList listQueryEngines()

## Get a reference to a node based on its identifier
## @param cnode The coordinating node to query for its registered Member Nodes
## @param nodeid The standard identifier string for this node
## @returnType MNode
## @return the Member Node as an MNode reference, or NULL if not found
## 
## @author jones
#' @export
setGeneric("getMNode", function(cnode, nodeid, ...) {
  standardGeneric("getMNode")
})

setMethod("getMNode", signature(cnode = "CNode", nodeid = "character"), function(cnode, nodeid) {
  nodelist <- listNodes(cnode)
  match <- sapply(nodelist, function(node) { 
    node@identifier == nodeid && node@type == "mn"
  })
  output.list <- nodelist[match]
  if (length(output.list) == 1) {
    mn <- MNode(output.list[[1]])
    return(mn)  
  } else {
    return(NULL)
  }
})


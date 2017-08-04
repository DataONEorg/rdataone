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
#' @title Provides R API to DataONE Coordinating Node services.
#' @description The CNode class provides methods that interact with a DataONE Coordinating Node.
#' @rdname CNode-class
#' @aliases CNode-class
#' @slot endpoint A character vector containing URL service endpoint for the Coordinating Node
#' @slot services A data.frame containing the supported service tiers for a CN
#' @slot serviceUrls A data.frame contains URL endpoints for certain services
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{CNode}}}{: Construct a CNode object.}
#'  \item{\code{\link{listFormats}}}{: List all object formats registered in DataONE.}
#'  \item{\code{\link{getFormat}}}{: Get information for a single DataONE object format } 
#'  \item{\code{\link{getChecksum}}}{: Get the checksum for the data object associated with the specified pid.}
#'  \item{\code{\link{listNodes}}}{: Get the list of nodes associated with a CN.} 
#'  \item{\code{\link{reserveIdentifier}}}{: Reserve a identifier that is unique in the DataONE network.}
#'  \item{\code{\link{hasReservation}}}{: Checks to determine if the supplied subject is the owner of the reservation of id.}
#'  \item{\code{\link{setObsoletedBy}}}{: Set a pid as being obsoleted by another pid}
#'  \item{\code{\link{getObject}}}{: Get the bytes associated with an object on this Coordinating Node.} 
#'  \item{\code{\link{getSystemMetadata}}}{: Get the bytes associated with an object on this Coordinating Node.}
#'  \item{\code{\link{describeObject}}}{: Get a list of coordinating nodes holding a given pid.} 
#'  \item{\code{\link{resolve}}}{: Get a list of coordinating nodes holding a given pid.}
#'  \item{\code{\link{getMNode}}}{: Get a reference to a node based on its identifier.} 
#'  \item{\code{\link{echoCredentials}}}{: Echo the credentials used to make the call.} 
#'  \item{\code{\link{isAuthorized}}}{: Check if an action is authorized for the specified identifier.} 
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @import methods
#' @importFrom utils URLencode
#' @export
setClass("CNode", slots = c(endpoint = "character"), contains="D1Node")

#########################
## CNode constructors
#########################

#' Create a CNode object.
#' @details For an explanation of DataONE Coordinating Nodes, see the 
#' section \emph{"DataONE Environments"} in the overview vignette by entering the R command: \code{vignette("dataone-overview")}.
#' @param x The label for the DataONE environment to be using ('PROD','STAGING', 'STAGING2,'SANDBOX', 'SANDBOX2','DEV', 'DEV2')
#' @param ... (not yet used)
#' @rdname CNode
#' @aliases CNode
#' @return the CNode object representing the DataONE environment
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
#' @examples
#' cn <- CNode("PROD")
setGeneric("CNode", function(x, ...) {
  standardGeneric("CNode")
})

#' @rdname CNode
#' @export
setMethod("CNode", signature=character(), function() {
    result <- CNode("PROD")
    return(result)
})

#' @rdname CNode
#' @export
setMethod("CNode", signature("character"), function(x) {

  ## Define the default CNs for each environment.
  PROD <- "https://cn.dataone.org/cn"
  STAGING <- "https://cn-stage.test.dataone.org/cn"
  STAGING2 <- "https://cn-stage-2.test.dataone.org/cn"
  SANDBOX <- "https://cn-sandbox.test.dataone.org/cn"
  SANDBOX2 <- "https://cn-sandbox-2.test.dataone.org/cn"
  DEV <- "https://cn-dev.test.dataone.org/cn"
  DEV2 <- "https://cn-dev-2.test.dataone.org/cn"

  # By default, use production.  But also look in the environment.
  CN_URI <- PROD
  cn.url <- Sys.getenv("CN_URI")
  if(cn.url != "") {
    CN_URI <- cn.url
  } else {
    if (x == "DEV") CN_URI <- DEV
    else if (x == "DEV2") CN_URI <- DEV2
    else if (x == "STAGING") CN_URI <- STAGING
    else if (x == "STAGING2") CN_URI <- STAGING2
    else if (x == "SANDBOX") CN_URI <- SANDBOX
    else if (x == "SANDBOX2") CN_URI <- SANDBOX2
    else if (x == "PROD") CN_URI <- PROD
    else stop(sprintf("Unknown DataONE environment: %s", x))
  }

  ## create new D1Client object and insert uri endpoint
  result <- new("CNode")
  # Get the node listing for just this CN using just the baseURL, as we don't know the API version number
  # yet that is needed to construct the service URL.
  response <- GET(CN_URI)   
  if(response$status != "200") {
    stop(sprintf("Error accessing %s: %s\n", CN_URI, getErrorDescription(response)))
  }
  # Search for the 'node' element. Have to search for local name in xpath, as DataONE v1 and v2 use different namespaces
  # and one xpath expression with namespaces can't find both (that I know of).
  xml <- getNodeSet(xmlParse(content(response, as="text")), "/*[local-name() = 'node']")
  result <- dataone:::parseCapabilities(result, xml[[1]])
  result@baseURL <- CN_URI
  result@endpoint <- paste(result@baseURL, result@APIversion, sep="/")
  # Set the service URL fragment for the solr query engine
  result@serviceUrls <- data.frame(service="query.solr", Url=paste(result@endpoint, "query", "solr", "?", sep="/"), row.names = NULL, stringsAsFactors = FALSE)
  if(x == "PROD") {
    result@env <- "prod"
  } else {
    result@env <- "test"
  }

  return(result)
})

##########################
## Methods
##########################

#' List all object formats registered in DataONE.
#' @description The \link{listFormats} method queries a DataONE Coordinating Node for a 
#' list of all entries in the Object Format Vocabulary.
#' @param x a valid CNode object
#' @param ... (Not yet used)
#' @import httr
#' @rdname listFormats
#' @aliases listFormats 
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @return Returns a dataframe of all object formats registered in the DataONE Object Format Vocabulary.
#' @examples
#' library(dataone)
#' cn <- CNode()
#' formats <- listFormats(cn)
#' @export
setGeneric("listFormats", function(x, ...) {
  standardGeneric("listFormats")
})

#' @rdname listFormats
#' @import plyr
#' @export
setMethod("listFormats", signature("CNode"), function(x) {
  url <- paste(x@endpoint,"formats",sep="/")
  response <- GET(url, user_agent(get_user_agent()))
  # Use charset 'utf-8' if not specified in response headers
  charset <- "utf-8"
  if("content-type" %in% names(response$headers)) {
      media <- parse_media(response$headers[['content-type']])
      if("params" %in% names(media) && "charset" %in% names(media$params)) {
          charset <- media$params$charset
      }
  }  
  
  out <- xmlToList(xmlParse(content(response, as="text", encoding=charset)))
  ## Below could be done with plyr functionality, but I want to reduce
  ## dependencies in the package
  #df <- data.frame(matrix(NA,ncol=length(out[[1]]),nrow=(length(out)-1)))
  df <- data.frame(out[[1]], stringsAsFactors = F)
  dfNames <- colnames(out[[1]])
  for(i in 2:(length(out)-1)){
    df <- rbind.fill(df, data.frame(out[[i]], stringsAsFactors = F))
    currentNames <- names(out[[i]])
    if(length(currentNames) > length(dfNames)) {
      dfNames <- currentNames
    }
    #df[i,] <- c(unlist(out[[i]]))
  }
  
  # For v2, continue to use the columnames that were used for v1, and add the
  # new column names availabie in v2.
  # from v1: colnames(df) <- c("ID","Name","Type")
  colnames(df) <- dfNames
  # Use replace because we don't want to assume the order of the names in the source data.
  dfNames <- replace(dfNames, dfNames=="formatId", "ID")
  dfNames <- replace(dfNames, dfNames=="formatName", "Name")
  dfNames <- replace(dfNames, dfNames=="formatType", "Type")
  dfNames <- replace(dfNames, dfNames=="mediaType", "MediaType")
  dfNames <- replace(dfNames, dfNames=="extension", "Extension")
  names(df) <- dfNames
  return(df)
})

#' Get information for a single DataONE object format
#' @rdname getFormat
#' @aliases getFormat
#' @param x A CNode object instance
#' @param ... (Not yet used)
#' @return A dataframe of all object formats registered in the DataONE Object Format Vocabulary.
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @examples
#' library(dataone)
#' cn <- CNode()
#' fmt <- getFormat(cn, "eml://ecoinformatics.org/eml-2.1.0")
#' cat(sprintf("format name: %s\n", fmt$name))
#' cat(sprintf("format type: %s\n", fmt$type))
#' cat(sprintf("format Id: %s\n", fmt$id))
#' @export
setGeneric("getFormat", function(x, ...) {
  standardGeneric("getFormat")
})

#' @rdname getFormat
#' @param formatId The formatId to retrieve.
#' @export
setMethod("getFormat", signature("CNode"), function(x, formatId) {
  url <- paste(x@endpoint,"formats", URLencode(formatId, reserved=T), sep="/")
  response <- GET(url, user_agent(get_user_agent()))
  
  if(response$status != "200") {
    return(NULL)
  }
  
  result <- xmlToList(xmlParse(content(response,as="text")))
  fmt <- list(name=result$formatName, type=result$formatType, id=result$formatId)
  # Add DataONE v2 types if present
  if(is.element("mediaType", names(result))) fmt["mediaType"] <- result[["mediaType"]]
  if(is.element("extension", names(result))) fmt["extension"] <- result[["extension"]]
  
  return(fmt)
})

#' @rdname getChecksum
#' @export
#' @examples
#' pid <- "doi:10.5063/F1QN64NZ"
#' cn <- CNode()
#' pid <- "doi:10.5063/F1QN64NZ"
#' chksum <- getChecksum(cn, pid)
setMethod("getChecksum", signature("CNode"), function(x, pid, ...) {
  url <- paste(x@endpoint, "checksum", URLencode(pid, reserved=T), sep="/")
  response <- GET(url, user_agent(get_user_agent()))
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

# @see https://purl.dataone.org/architecture/apis/CN_APIs.html#CNCore.getLogRecords
# public Log getLogRecords(Date fromDate, Date toDate, Event event, String pidFilter, Integer start, Integer count) 

#' Get the list of nodes associated with a CN
#' @rdname listNodes
#' @aliases listNodes
#' @param x The coordinating node to query for its registered Member Nodes
#' @param ... (Not yet used)
#' @return the list of nodes in the DataONE CN environment
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
#' @examples 
#' \dontrun{
#' cn <- CNode()
#' nodelist <- listNodes(cn)
#' nodeid <- nodelist[[2]]@identifier
#' }
setGeneric("listNodes", function(x, ...) {
    standardGeneric("listNodes")
})

#' @rdname listNodes
#' @param url Optianal - the url of the CN.
#' @export
setMethod("listNodes", signature("CNode"), function(x, url=as.character(NA), ...) {
    # If an optional url argument is specified, use that. This might be used if
    # we are listing just the CN itself, and don't know the version number, i.e.
    # "https://cn-dev.test.dataone.org/cn" gvies a listing for just the CN
    if(is.na(url)) {
      url <- paste(x@endpoint, "node", sep="/")
    }
    # Don't need authorized access, so call GET directly vs auth_get
    response <- GET(url)
    if(response$status != "200") {
        return(NULL)
    }
    
    # Use charset 'utf-8' if not specified in response headers
    charset <- "utf-8"
    if("content-type" %in% names(response$headers)) {
        media <- parse_media(response$headers[['content-type']])
        if("params" %in% names(media) && "charset" %in% names(media$params)) {
            charset <- media$params$charset
        }
    } 
    
    xml <- xmlParse(content(response, as="text", encoding=charset))
    #node_identifiers <- sapply(getNodeSet(xml, "//identifier"), xmlValue)
    nodes <- getNodeSet(xml, "//node")
    nodelist <- sapply(nodes, D1Node)
    return(nodelist)
})

#' Reserve a identifier that is unique in the DataONE network.
#' @description The reserveIdentifier method contains the DataONE CN and reserves the specified
#' identifier that the user has provided. Once a an identifier has been reserved, it and can not be used by any other user.
#' @details This method requires a DataONE authentication token or X.509 Certificate. The reservation is made
#' for the DataONE user identity that created the current authentication token or X.509 certificate.
#' @rdname reserveIdentifier
#' @aliases reserveIdentifier
#' @param x The coordinating node to query for its registered Member Nodes
#' @param ... Additional parameters.
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
#' @examples
#' \dontrun{
#' library(dataone)
#' library(uuid)
#' cn <- CNode("STAGING")
#' myId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' newId <- reserveIdentifier(cn, myId)
#' }
setGeneric("reserveIdentifier", function(x, ...) {
  standardGeneric("reserveIdentifier")
})

#' @rdname reserveIdentifier
#' @param id The identifier that is to be reserved.
#' @return The reserved pid if it was successfully reserved, otherwise NULL
setMethod("reserveIdentifier", signature("CNode"), function(x, id) {
  url <- paste(x@endpoint, "reserve", sep="/")
  response <- auth_post(url, encode="multipart", body=list(pid=id), node=x)
  # Note: the DataONE reserveIdentifier service uses the subject from the client certificate
  # as the subject to reserve the identifier for.
  if(response$status != "200") {
      warning(sprintf("Error reserving identifier %s: %s\n", id, getErrorDescription(response)))
    return(NULL)
  } else {
    resultText <- content(response, as="text")
    doc <- xmlInternalTreeParse(resultText)
    # XML doc is similiar to: <d1:identifier xmlns:d1="http://ns.dataone.org/service/types/v1">WedSep91341002015-ub14</d1:identifier>
    nodes <- getNodeSet(doc, "/*[local-name() = 'identifier']")
    id <- xmlValue(nodes[[1]])
    # Return the identifier as a character value
    return(id)
  }
})

#' Checks to determine if the supplied subject is the owner of the reservation of id.
#' @description The hasReservation method checks the reservation of an identfier that has
#' previously been reserved with the \code{reserveIdentifier} method. The identifier must have
#' been reserved by the specified DataONE user identity (\code{subject}).
#' @details To determine the DataONE identity that is currently being used for DataONE
#' authentication, use the \code{echoCredentials} method.
#' @param x A CNode instance.
#' @param ... Additional parameters.
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
#' @examples
#' \dontrun{
#' library(dataone)
#' cn <- CNode("STAGING")
#' creds <- echoCredentials(cn)
#' subject <- creds$person$subject
#' # Previously reserved pid (using reserveIdentifeir()), e.g. DOI or uuid
#' pid <- "urn:node:e27bb4f3-96bb-4af4-8902-f5914def077c"
#' hasRes <- hasReservation(cn, pid, subject=subject)
#' }
setGeneric("hasReservation", function(x, ...) {
  standardGeneric("hasReservation")
})

#' @rdname hasReservation
#' @param pid The identifier that is being checked for existing as a reserved identifier or is in use as 
#' an identifier for an existing object
#' @param subject The subject of the principal (user) that made the reservation.
#' @return A logical value where TRUE means a reservation exists for the specified pid by the subject.
setMethod("hasReservation", signature("CNode"), function(x, pid, subject=as.character(NA)) {
  stopifnot(is.character(pid))
  url <- paste(x@endpoint, "reserve", URLencode(pid, reserved=T), sep="/")
  # Obtain the subject from the client certificate if it has not been specified
  if(is.na(subject)) {
    am <- AuthenticationManager()
    if(isAuthValid(am, x)) {
      subject <- getAuthSubject(am, x)
    }
    if(is.na(subject)) {
      warning("Unable to determine subject for hasReservation(), please specify \"subject\" parameter")
    }
  }
  # The subject might contain '=', so encode reserved chars also.
  url <- sprintf("%s?%s", url, sprintf("subject=%s", URLencode(subject, reserved=TRUE)))
  response <- auth_get(url, node=x)
  # The DataONE 'hasReservation' service uses the HTTP status code to communicate the
  # existence of a reservation for the pid and subject combination. The following HTTP status
  # codes and their meaning are shown below:
  #     Status code      meaning
  #     200              A reservation for the pid and subject combination exists
  #     401              Unauthorized - pid reservation exists, but subject doesn't have privilege
  #                      to access it
  #     404              A reservation for the pid does not exist
  # 
  if(response$status != "200") {
      warning(sprintf("Error checking reservation for pid=%ssubject=%s: %s\n", 
                 pid, subject, getErrorDescription(response)))
    return(FALSE)
  }
  # When a reservation exists, there is nothing of interest in the response, so just return
  # a TRUE value.
  return(TRUE)
})

#' Set a pid as being obsoleted by another pid
#' @description Updates the SystemMetadata 'obsoletedBy' property for an object, indicating that the object 
#' specified by pid has been obsoleted by the identifier in obsoletedByPid.
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{CertificateManager}} for details.
#' In DataONE version 2.0, authentication tokens can also be used.
#' @rdname setObsoletedBy
#' @aliases setObsoletedBy
#' @param x The CNode instance on which the object will be created
#' @param pid The identifier of the object to be obsoleted
#' @param obsoletedByPid The identifier of the object that obsoletes the object identified by pid.
#' @param serialVersion The serial version of the system metadata of the pid being obsoleted. 
#' @param ... (Not yet used)
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
setGeneric("setObsoletedBy", function(x, pid, obsoletedByPid, ...) {
  .Defunct("updateObject", "dataone")
  standardGeneric("setObsoletedBy")
})

#' @rdname setObsoletedBy
#' @return TRUE if the pid was obsoleted, otherwise FALSE is returned
setMethod("setObsoletedBy", signature("CNode", "character"), function(x, pid, obsoletedByPid, serialVersion) {
  url <- paste(x@endpoint, "obsoletedBy", URLencode(pid, reserved=TRUE), sep="/")
  body=list(obsoletedByPid=URLencode(obsoletedByPid), serialVersion=serialVersion)
  response <- auth_put(url=url, body=body, node=x)
  if(response$status != "200") {
      warning(sprintf("Error obsoleting %s: %s\n", pid, getErrorDescription(response)))
    return(FALSE)
  } else {
    return(TRUE)
  }
})

#' @rdname getObject
setMethod("getObject", signature("CNode"), function(x, pid) {
    url <- paste(x@endpoint, "object", URLencode(pid, reserved=T), sep="/")
    response <- auth_get(url, node=x)
    
    if(response$status != "200") {
        warning(sprintf("Error getting pid: %s\n", getErrorDescription(response)))
        return(NULL)
    }
    
    return(content(response, as="raw"))
})

#' Get the metadata describing system properties associated with an object on a Coordinating Node.
#' @description The SystemMetadata includes information about the identity, type, access control, and other system
#' level details about the object.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @return SystemMetadata for the object
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @import datapack
#' @export
#' @rdname getSystemMetadata
#' @examples
#' library(dataone)
#' cn <- CNode()
#' pid <- "aceasdata.3.2"
#' sysmeta <- getSystemMetadata(cn, pid)
setMethod("getSystemMetadata", signature("CNode"), function(x, pid) {
  stopifnot(is.character(pid))
    # TODO: need to properly URL-escape the PID
    url <- paste(x@endpoint, "meta", URLencode(pid, reserved=T), sep="/")
    response <- auth_get(url, node=x)
    
    if(response$status != "200") {
      warning(sprintf("Error getting SystemMetadata: %s\n", getErrorDescription(response)))
        return(NULL)
    }
    
    # Convert the response into a SystemMetadata object
    sysmeta <- SystemMetadata(xmlRoot(xmlParse(content(response, as="text"))))
    
    return(sysmeta)
})

#' Get a list of coordinating nodes holding a given pid.
#' @description Returns a list of nodes (MNs or CNs) known to hold copies of the object identified by id.
#' @param x a valid CNode object
#' @param ... Additional arguments (not yet used).
#' @param pid the id of the identified object
#' @rdname resolve
#' @aliases resolve
#' @return A list of URLs that the object can be downloaded from, or NULL if the object is not found.
#' @examples
#' \dontrun{
#' library(dataone)
#' cn <- CNode("STAGING")
#' id <- "doi:10.6073/pasta/9a27a1615e8e4c366ad09fefbfa2fced"
#' locations <- resolve(cn,id)
#' }
#' @export
setGeneric("resolve", function(x, ...) {
  standardGeneric("resolve")
})

#' @rdname resolve
#' @export
setMethod("resolve", signature("CNode"), function(x, pid){
  stopifnot(is.character(pid))
  url <- paste(x@endpoint,"resolve",URLencode(pid, reserved=T),sep="/")
  config <- c(add_headers(Accept = "text/xml"), config(followlocation = 0L))
  res <- auth_get(url, nconfig=config, node=x)
  # Check if there was an error downloading the object
  # The DataONE resolve service returns HTTP status 303, which essentially
  # means the response contains a URI to the object that was requested and
  # not the object itself. In this case of the resolve service, a list of
  # MN URLs for the requested object are returned.
  if (res$status_code != 303) {
    errorMsg <- getErrorDescription(res)
    message(sprintf('Error resolving url "%s": "%s".\n', url, errorMsg))
    return(NULL)
  }
      
  # FYI The XML response from dataone looks something like this:
  # <?xml version="1.0" encoding="UTF-8"?>
  # <d1:objectLocationList xmlns:d1="http://ns.dataone.org/service/types/v1">
  #  <identifier>urn:uuid:c4c610c9-460a-45a0-8039-6a50e149f8d6</identifier>
  #  <objectLocation>
  #    <nodeIdentifier>urn:node:mnDemo2</nodeIdentifier>
  #    <baseURL>https://mn-demo-2.test.dataone.org/metacat/d1/mn</baseURL>
  #    <version>v1</version>
  #    <version>v2</version>
  #    <url>https://mn-demo-2.test.dataone.org/metacat/d1/mn/v2/object/urn:uuid:c4c610c9-460a-45a0-8039-6a50e149f8d6</url>
  #  </objectLocation>
  # </d1:objectLocationList>
  out <- xmlToList(xmlParse(content(res,as="text")))
  # For the parsed XML:
  #   Index 1 is the identifier we are resolving
  #   Index 2-n are the objectLocations
  # Number of columns may vary depending on whether this is a
  # v1 mn or v2
  df <- data.frame(nodeIdentifier=character(), baseURL=character(), url=character(), stringsAsFactors = F)
  # Using a loop when plyr would work to reduce dependencies.
  for(i in 2:length(out)){
    df <- rbind(df, data.frame(nodeIdentifier=out[[i]]$nodeIdentifier, 
                               baseURL=out[[i]]$baseURL,
                               url=out[[i]]$url, stringsAsFactors = F))
  }
  
  toret <- list(id = pid, data = df)
  return(toret)
})
    
#' Get a reference to a node based on its identifier
#' @rdname getMNode
#' @aliases getMNode
#' @details For an explanation of DataONE Coordinatine Nodes and Member Node
#' identifiers, see the section \emph{"DataONE Environments"} in the overview vignette 
#' by entering the R command: \code{vignette("dataone-overview")}.
#' @param x The coordinating node to query for its registered Member Nodes
#' @param nodeid The standard identifier string for this node
#' @param ... (Not yet used)
#' @return the Member Node as an MNode reference, or NULL if not found
#' @seealso \code{\link[=CNode-class]{CNode}}{ class description.}
#' @export
#' @examples
#' cn <- CNode()
#' mn <- getMNode(cn, "urn:node:KNB")
setGeneric("getMNode", function(x, ...) {
  standardGeneric("getMNode")
})

#' @rdname getMNode
setMethod("getMNode", signature(x = "CNode"), function(x, nodeid) {
  stopifnot(is.character(nodeid))
  nodelist <- listNodes(x)
  match <- sapply(nodelist, function(x) { 
    x@identifier == nodeid && x@type == "mn"
  })
  output.list <- nodelist[match]
  if (length(output.list) == 1) {
    thisNode <- output.list[[1]]
    # See if this cn is the production cn. If yes
    # then mark this node as being in the production
    # environment.
    if (grepl("cn.dataone.org", x@endpoint)) {
      thisNode@env <- "prod"
    } else {
      thisNode@env <- "test"
    }
    mn <- MNode(thisNode)
    return(mn)  
  } else {
    warning(sprintf("Member node %s not found.", nodeid))
    return(NULL)
  }
})

#' Echo the credentials used to make the call. 
#' @description This method can be used to verify the client certificate is valid 
#' and contains the expected information.
#' @details The authentication credentials contained in the X.509 certificate or
#' authentication token are send with the request.
#' @rdname echoCredentials
#' @aliases echoCredentials
#' @param x The coordinating node to send the request to.
#' @param ... (Not yet used)
#' @return A list containing authentication info.
#' @export
#' @examples \dontrun{
#' cn <- CNode("STAGING")
#' creds <- echoCredentials(cn)
#' print(creds$person$subject)
#' }
setGeneric("echoCredentials", function(x, ...) {
  standardGeneric("echoCredentials")
})

#' @rdname echoCredentials
setMethod("echoCredentials", signature(x = "CNode"), function(x) {
  url <- sprintf("%s/diag/subject", x@endpoint)
  response <- auth_get(url, node=x)
  if(response$status != "200") {
    warning(sprintf("Error checking credentails %s", getErrorDescription(response)))
    return(as.character(NA))
  }
  result <- xmlToList(xmlParse(content(response,as="text")))
  return(result)
})

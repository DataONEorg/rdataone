#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2015
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
#' @title The D1Client class contains methods that perform high level DataONE tasks
#' @description The methods in the D1Client class call the low level DataONE API to
#' perform involved tasks such as uploading all the packages in a DataPackage (i.e
#' \code{\link{uploadDataPackage}})
#' @include CNode.R
#' @include MNode.R
#' @include D1Object.R
#' @rdname D1Client-class
#' @aliases D1Client-class
#' @slot cn The Coordinating Node associated with the D1Client object
#' @slot mn The Member Node associated with this D1Client object
#' @import datapack
#' @importFrom stringr str_match str_replace
#' @importFrom utils URLencode
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{D1Client}}}{: Construct a D1Client object.}
#'  \item{\code{\link{convert.csv}}}{: Convert a DataFrame to Standard CSV.}
#'  \item{\code{\link{createDataPackage}}}{: Create a DataPackage on a DataONE Member Node.}
#'  \item{\code{\link{encodeUrlPath}}}{: Encode the Input for a URL Path Segment.}
#'  \item{\code{\link{encodeUrlQuery}}}{: Encode the Input for a URL Query Segment.}
#'  \item{\code{\link{getDataObject}}}{: Download a single data object from a DataONE Federation member node.}
#'  \item{\code{\link{getDataPackage}}}{: Download a collection of data object from the DataONE Federation member node as a DataPackage.}
#'  \item{\code{\link{getEndpoint}}}{: Return the URL endpoint for the DataONE Coordinating Node.}
#'  \item{\code{\link{getMetadataMember}}}{: Get the DataObject containing package metadata.}
#'  \item{\code{\link{getMNodeId}}}{: Get the member node identifier associated with this D1Client object.}
#'  \item{\code{\link{listMemberNodes}}}{: List DataONE Member Nodes.}
#'  \item{\code{\link{reserveIdentifier}}}{: Reserve a unique identifier in the DataONE Network.}
#'  \item{\code{\link{uploadDataObject}}}{: Upload a DataObject to a DataONE member node.}
#'  \item{\code{\link{uploadDataPackage}}}{: Upload a DataPackage to a DataONE member node.}
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @export
#' @import datapack
setClass("D1Client", slots = c(cn = "CNode", mn="MNode"))

#########################
## D1Client constructors
#########################

#' The DataONE client class used to download, update and search for data in the DataONE network.
#' @rdname D1Client
#' @param x The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV'). This parameter
#' can alternatively be a \code{\link{CNode}} instance, with the `y` parameter specified as an \code{\link{MNode}} instance.
#' @param y The node Id of the application's 'home' node.  Should be already registered to the corresponding 'env'. This
#' parameter can alternatively be an \code{\link{MNode}} instance, with the `x` parameter specified as a \code{\link{CNode}} instance.
#' @param ... (not yet used)
#' @return the D1Client object representing the DataONE environment
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' cli <- D1Client("PROD", "urn:node:KNB")
#' cn <- CNode('STAGING2')
#' mn <- getMNode(cn,'urn:node:mnTestKNB')
#' cli <- D1Client(cn,mn)
#' }
setGeneric("D1Client", function(x, y, ...) {
    standardGeneric("D1Client")
})

#' @rdname D1Client
#' @export
setMethod("D1Client", signature=character(), function() {
    result <- D1Client("PROD", "")
    return(result)
})

#' @rdname D1Client
#' @export
setMethod("D1Client", signature("character"), function(x, ...) {
    #message("Instantiating D1Client without a default Member Node.")
    result <- D1Client(x, "")
    return(result)
})

#' @rdname D1Client
#' @export
setMethod("D1Client", signature("character", "character"), function(x, y) {
    result <- new("D1Client", env=x, mNodeid=y)
    return(result)
})

#' @rdname D1Client
#' @export
setMethod("D1Client", signature("CNode", "MNode"), function(x, y, ...) {
    result <- new("D1Client", cn=x, mn=y)
    return(result)
})

#' Initialize a D1Client object
#' @param .Object A D1client object.
#' @param cn The Member Node object to associate this D1Client with.
#' @param mn The Member Node object to associate this D1Client with.
#' @param env The DataONE environment to initialize this D1Client with, e.g. "PROD", "STAGING", "SANDBOX", "DEV"
#' @param mNodeid The node identifier of the Member Node to associate with this D1Client.
#' @rdname D1Client-initialize
#' @aliases D1Client-initialize
#' @export
#' @examples \dontrun{
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' }
#' @seealso \code{\link[=D1Client-class]{dataone}}{ class description.}
setMethod("initialize", signature = "D1Client", definition = function(.Object, cn=NA, mn=NA, env=as.character(NA), mNodeid=as.character(NA)) {
    # defaults here
    if (missing(cn)) {
        .Object@cn <- CNode()
    } else {
        .Object@cn <- cn
    }
    if (!missing(mn)) {
        .Object@mn <- mn
    }
    if (!missing(env)) {
        .Object@cn <- CNode(env)
    }
    if (!missing(mNodeid) && !is.na(mNodeid) && !is.null(mNodeid) && (nchar(mNodeid) > 0)) {
      .Object@mn <- getMNode(.Object@cn, mNodeid)
    }
    return(.Object)
})

#' Create the Object in the DataONE System
#' 
#' @param x : D1Client
#' @param d1Object A D1Object instance to upload to DataONE 
#' @param ... (not yet used)
#' @rdname createD1Object
#' @aliases createD1Object
#' @return TRUE if the object was successfully uploaded, FALSE if not.
#' @export
#' @examples \dontrun{
#' library(dataone)
#' library(uuid)
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' data <- readLines(system.file("extdata/strix-pacific-northwest.xml", package="dataone"))
#' dataRaw <- charToRaw(paste(data, collapse="\n"))
#' newid <- sprintf("urn:node:%s", UUIDgenerate())
#' d1o <- new("D1Object", id=newid, data=dataRaw, format="text/plain")
#' d1o <- setPublicAccess(d1o)
#' # Upload the object to DataONE (requires authentication)
#' uploaded <- createD1Object(d1c, d1o)
#' }
setGeneric("createD1Object", function(x, d1Object, ...) {
  .Deprecated("uploadDataObject", "datapack")
  standardGeneric("createD1Object")
})

#' @rdname createD1Object
setMethod("createD1Object", signature("D1Client", "D1Object"), function(x, d1Object) {
  newId <- uploadDataObject(x, d1Object@dataObject)
  if (!is.null(newId)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
})

#' Download a data object from the DataONE Federation.
#' @description An object is download from the DataONE network for the identifier that is provided.
#' @param x A D1Client instance
#' @param identifier The identifier of the object to download from DatONE
#' @param ... (not yet used)
#' @rdname getD1Object
#' @aliases getD1Object
#' @return A datapack:DataObject
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{ 
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' pid <- "solson.5.1"
#' dataObj <- getD1Object(d1c, pid)
#' data <- getData(dataObj)
#' }
setGeneric("getD1Object", function(x, identifier, ...) {
  .Deprecated("getDataObject", "dataone")
  standardGeneric("getD1Object")
})

#' @rdname getD1Object
setMethod("getD1Object", "D1Client", function(x, identifier) {
  #d1o <- get(x@cn, identifier)    # Resolve the object location
  return(getDataObject(x, identifier))
})

#' Download a from the DataONE Federation as a DataObject.
#' @description A convenience method to download a data object and its associated SystemMetadata, wrapped
#' in a DataObject class.
#' @details This method performs multiple underlying calls to the DataONE repository network. 
#' CN.resolve() is called to locate the object on one or more repositories, and then each of these
#' is accessed until success at downloading the associated SystemMetadata and data bytes, which are 
#' finally wrapped in a DataObject and returned. Replaces previous getD1Object() method in the version 1
#' dataone library. The \code{lazyLoad} parameter specifies that only sysmeta metadata is downloaded and
#' not the data itself. This argument is used together with the \code{limit} parameter, which specifies 
#' the maximum size of data object that will be downloaded. IF \code{lazyLoad} is FALSE, then \code{limit}
#' is ignored.
#' @param x A D1Client object.
#' @param identifier The identifier of the object to get.
#' @param lazyLoad A \code{logical} value. If TRUE, then only package member system metadata is downloaded and not data.
#' @param limit A \code{character} value specifying maximum package member size to download. Specified with "KB", "MB" or "TB"
#'              for example: "100KB", "10MB", "20GB", "1TB". The default is "1MB".
#' @param quiet A \code{'logical'}. If TRUE (the default) then informational messages will not be printed.
#' @param ... (not yet used)
#' @rdname getDataObject
#' @aliases getDataObject
#' @return A DataObject or NULL if the object was not found in DataONE
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' pid <- "solson.5.1"
#' obj <- getDataObject(d1c, pid)
#' data <- getData(obj)
#' }
setGeneric("getDataObject", function(x, identifier, ...) { 
    standardGeneric("getDataObject")
})

#' @rdname getDataObject
#' @export
setMethod("getDataObject", "D1Client", function(x, identifier, lazyLoad=FALSE, limit="1MB", quiet=TRUE) {
    
    # Resolve the object location
    # This service is too chatty if any of the locations aren't available
    suppressMessages(result <- resolve(x@cn, identifier))
    if(is.null(result)) {
      #message("Unable to download object with identifier: %s\n", identifier)
      #return(NULL)
      # If the object isn't resolved from the CN, then try the member node directly. Fill out the
      # mntable as if the member node had returned the object.
      mntable <- data.frame(nodeIdentifier=x@mn@identifier, 
                            baseURL=x@mn@baseURL,
                            url=sprintf("%s/%s/object/%s", x@mn@baseURL, x@mn@APIversion, 
                                        URLencode(identifier, reserved=TRUE)), 
                            row.names=NULL, stringsAsFactors=FALSE)
    } else {
      mntable <- result$data
    }
    
    # Convert download size limit to a number. This will only be used if lazyLoad=TRUE
    if(grepl("tb", limit, ignore.case=TRUE)) {
      limitBytes <- as.numeric(gsub("tb", "", limit, ignore.case=TRUE)) * 1099511627776
    } else if(grepl("gb", limit, ignore.case=TRUE)) {
      limitBytes <- as.numeric(gsub("gb", "", limit, ignore.case=TRUE)) * 1073741824
    } else if(grepl("mb", limit, ignore.case=TRUE)) {
      limitBytes <- as.numeric(gsub("mb", "", limit, ignore.case=TRUE)) * 1048576
    } else if (grepl("kb", limit, ignore.case=TRUE)) {
      limitBytes <- as.numeric(gsub("kb", "", limit, ignore.case=TRUE)) * 1024
    } else if(!is.na(as.numeric(x))) {
      limitByptes <- as.numeric(limit)
    } else {
      stop(sprintf("Unknown limit specified: %s", limit))
    }
    
    # Get the SystemMetadata and object bytes from one of the MNs
    # Process them in order, until we get non-NULL responses from a node
    sysmeta <- NA
    bytes <- NA
    success <- FALSE
    dataURL <- as.character(NA)
    deferredDownload <- lazyLoad
    if(nrow(mntable) > 0) {
      for (i in 1:nrow(mntable)) { 
        suppressWarnings(currentMN <- getMNode(x@cn, mntable[i,]$nodeIdentifier))
        # If cn couldn't return the member node, then fallback to the D1Client@mn
        if(is.null(currentMN)) currentMN <- x@mn
        if (!is.null(currentMN)) {
          sysmeta <- getSystemMetadata(currentMN, identifier)
          if(is.null(sysmeta)) next
          # If lazy loading, check if the object size is larger that the specified
          # download threshold for loading now.
          if(!lazyLoad) { 
            deferredDownload <- FALSE
            bytes <- getObject(currentMN, identifier)
            if (!is.null(sysmeta) & !is.null(bytes)) {
              success <- TRUE
              dataURL <- mntable[i,]$url
              break
            }
          } else {
            if(as.numeric(sysmeta@size) <= limitBytes) {
              deferredDownload <- FALSE
              bytes <- getObject(currentMN, identifier)
              if (!is.null(sysmeta) & !is.null(bytes)) {
                success <- TRUE
                dataURL <- mntable[i,]$url
                break
              }
            } else {
              # This object is larger than the download max size, so just set
              # the bytes to NULL - DataObject initialize() will have to be
              # told that lazyLoad = TRUE for this object.
              deferredDownload <- TRUE
              bytes <- NA
              success <- TRUE
              dataURL <- mntable[i,]$url
              break
            }
          }
        }
      } 
    }
    
    if(!success) {
       message(sprintf("Unable to download object with identifier: %s\n", identifier))
       return(NULL)
    }
    
    if(!quiet) {
      if(deferredDownload) {
        cat(sprintf("Lazy Loaded object at URL %s\n", dataURL))
      } else {
        cat(sprintf("Downloaded object at URL %s\n", dataURL))
      }
    }
    
    # Construct and return a DataObject
    # Notice that we are passing the existing sysmeta for this object via the 'id' parameter,
    # which will cause the DataObject to use this sysmeta and not generate a new one.
    do <- new("DataObject", id=sysmeta, dataobj=bytes, dataURL=dataURL)
    # Save the identifier that this object had on the repository. If this update is to be updated,
    # the old identifier is needed for the update processing on the repo.
    do@oldId <- getIdentifier(do)
    return(do)
})

#' Download data from the DataONE Federation as a DataPackage.
#' @description This is convenience method that will download all the members in a DataONE data package 
#' and insert them into a DataPackage, including associated SystemMetadata for each package
#' member.
#' @details A 'data package' that resides on a DataONE member node is defined as a collection of
#' digital objects that are described by a metadata document. The 
#' @param x A D1Client object.
#' @rdname getDataPackage
#' @aliases getDataPackage
#' @return A DataPackage or NULL if the package was not found in DataONE
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' pid <- "solson.5.1"
#' pkg <- getDataPackage(d1c, pid)
#' }
setGeneric("getDataPackage", function(x, identifier, ...) { 
    standardGeneric("getDataPackage")
})

#' @rdname getDataPackage
#' @param identifier The identifier of a package, package metadata or other package member
#' @param lazyLoad A \code{logical} value. If TRUE, then only package member system metadata is downloaded and not data. 
#' The default is \code{FALSE}.
#' @param limit A \code{character} value specifying maximum package member size to download. Specified with "KB", "MB" or "TB"
#'              for example: "100KB", "10MB", "20GB", "1TB". The default is "1MB".
#' @param quiet A \code{'logical'}. If TRUE (the default) then informational messages will not be printed.
#' @param ... (not yet used)
#' @export
setMethod("getDataPackage", "D1Client", function(x, identifier, lazyLoad=FALSE, limit="1MB", quiet=TRUE) {
    
  # The identifier provided could be the package id (resource map), the metadata id or a package member (data, etc)
  # The solr queries attempt to determine which id was specified and may issue additional queries to get all the
  # data, for example, the metadata solr record must be retrieved to obtain all the package members.
  resmapId <- as.character(NA)
  metadataPid <- as.character(NA)
  # First find the metadata object for a package. Try to get all required info, but not all record types have all 
  # these fields filled out.
  queryParamList <- list(q=sprintf('id:\"%s\"', identifier), fl='isDocumentedBy,resourceMap,documents,formatType')
  node <- x@cn
  result <- query(node, queryParamList, as="list")
  # Didn't get a result from the CN, query the MN directly. This may happen for several reasons including
  # a new package hasn't been synced to the CN, the package is in a dev environment where CN sync is off, etc.
  if(is.null(result) || length(result) == 0) {
    if(!quiet) cat(sprintf("Trying %s\n", x@mn@identifier))
    node <- x@mn
    result <- query(node, queryParamList, as="list")
    if(is.null(result)) {
      stop(sprintf("Unable to get response for identifier %s", identifier))
    }
  } 
  if (length(result) == 0) {
    stop(sprintf("Identifier %s not found on node %s or %s", identifier, x@cn@identifier, x@mn@identifier))
  }
  
  formatType <- result[[1]]$formatType[[1]]
  # Check if we have the metadata object, and if not, then get it. If a data object pid was specified, then it is possible that
  # it can be contained in mulitple packages. 
  if(formatType == "METADATA") {
      # We have the metadata object, which contains the list of package members in the 'documents' field
      metadataPid <- identifier
      packageMembers <- unlist(result[[1]]$documents)
      resmapId <- unlist(result[[1]]$resourceMap)
      # Might have multiple resmaps, this will be handled later.
  } else if(formatType == "RESOURCE") {
    resmapId <- identifier
    # Get the metadata object for this resource map
    queryParamList <- list(q=sprintf('resourceMap:\"%s\"', identifier), fq='formatType:METADATA+AND+-obsoletedBy:*', fl='id,documents')
    result <- query(node, queryParamList, as="list")
    if (length(result) == 0) {
      stop(sprintf("Unable to find unobsolted metadata object for identifier: %s on node %s", identifier, node@identifier))
    }
    metadataPid <- unlist(result[[1]]$id)
    packageMembers <- unlist(result[[1]]$documents)
    if (length(packageMembers) == 0) {
        packageMembers <- list()
    }
  } else {
    # This must be a package member, so get the metadata pid for the package
    metadataPid <- unlist(result[[1]]$isDocumentedBy)
    queryParamList <- list(q=sprintf('id:\"%s\"', metadataPid), fq='-obsoletedBy:*', fl='id,resourceMap,documents')
    result <- query(node, queryParamList, as="list")
    if (length(result) == 0) {
        stop(sprintf("Unable to find metadata object for identifier: %s on node %s", identifier, node@identifier))
    }
    packageMembers <- unlist(result[[1]]$documents)
    if (length(packageMembers) == 0) {
        packageMembers <- list()
    }
    
    resmapId <- unlist(result[[1]]$resourceMap)
  }
  
  # The Solr index can contain multiple resource maps that refer to our metadata object. There should be only
  # one current resource map that refers to this metadata, the others may be previous versions of the resmap
  # that are now obsolete. If multple resource map pids were returned, filter out the obsolete ones.
  if(length(resmapId) > 1) {
    if(!quiet) {
      cat(sprintf("Multiple resource maps for this identifier, will filter out obsolete ones.\n"))
    }
    quoteSetting <- getOption("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    newIds <- dQuote(unlist(resmapId))
    options(useFancyQuotes = quoteSetting)
    
    qStr <- sprintf("id:(%s)", paste(newIds, collapse=" OR "))
    queryParamList <- list(q=qStr, fq="-obsoletedBy:*", fl="id")
    result <- query(node, queryParamList, as="list")
    resmapId <- unlist(result)
    if(length(resmapId) == 0) {
      stop(sprintf("It appears that all resource maps that reference pid %s are obsoleted.", identifier))
    }
    if(!quiet) {
        if(length(resmapId) > 1) {
          resmapStr <- paste(resmapId, collapse=", ")
          cat(sprintf("The metadata identifier %s is referenced by more than one current resource map: %s", metadataPid, resmapStr))
        } else {
            cat(sprintf("Using resource map with identifier: %s\n", resmapId))
        }
    }
  }
  
  # The Solr index can contain multiple metadata pdis that document a data pid, so filter out the
  # obsoleted and archived ones and use the first one of the remaining pids.
  if(length(metadataPid) > 1) {
      if(!quiet) {
          cat(sprintf("Multiple metadata pids that document this identifier, will filter out obsolete ones.\n"))
      }
      quoteSetting <- getOption("useFancyQuotes")
      options(useFancyQuotes = FALSE)
      newIds <- dQuote(unlist(metadataPid))
      options(useFancyQuotes = quoteSetting)
      
      qStr <- sprintf("id:(%s)", paste(newIds, collapse=" OR "))
      queryParamList <- list(q=qStr, fq="-obsoletedBy:*", fl="id,documents")
      result <- query(node, queryParamList, as="list")
      if(length(result) == 0) {
          stop(sprintf("It appears that all metadata pids that document pid %s are obsoleted.", identifier))
      }
      
      metadataPid <- unlist(result[[1]]$id)
      packageMembers <- unlist(result[[1]]$documents)
      if(!quiet) {
          if(length(metadataPid) > 1) {
              metadataPidStr <- paste(metadataPid, collapse=", ")
              cat(sprintf("The specified identifier %s is documented by more than one current metadata pid: %s", metadataPid, metadataPidStr))
          } else {
              cat(sprintf("Using metadata object with identifier: %s\n", metadataPid))
          }
      }
  }
  
  if(!quiet) {
    cat(sprintf("Downloading package members for package with metadata identifier: %s\n", metadataPid))
  }
  
  dpkg <- new("DataPackage")
  # Solr can return multiple resource maps 
  resmapId <- unlist(resmapId)[[1]]
  dpkg@resmapId <- resmapId
  # Don't lazyload the metadata
  metadataObj <- getDataObject(x, identifier=metadataPid, lazyLoad=FALSE, quiet=quiet)
  if(length(packageMembers) > 0) {
    for (iPid in 1:length(packageMembers)) {
      thisPid <- packageMembers[[iPid]]
      if(thisPid == metadataPid) {
        if(!quiet) cat(sprintf("Skipping metadata object, already downloaded\n"))
        next
      }
      obj <- getDataObject(x, identifier=thisPid, lazyLoad=lazyLoad, limit=limit, quiet=quiet)
      # The metadata object will be added this first time addMember is called.
      # Note that the 'cito:documents' relationship should already be in the package
      # resource map, so don't add this relationship now.
      dpkg <- addMember(dpkg, obj)
    }
  } else if (is.na(metadataPid)) {
    message(sprintf("This package does not contain any members or metadata, resource map identifier: %s", resmapId))
    return(NULL)
  }
  
  # Add the metadata object to the package.
  dpkg <- addMember(dpkg, metadataObj)
  
  # Download the resource map, parse it and load the relationships into the DataPackage
  # Currently we only use the first resource map
  if(!quiet) cat(sprintf("Getting resource map with id: %s\n", dpkg@resmapId))
  resMapBytes <- getObject(x@mn, pid=resmapId)
  resMap <- new("ResourceMap", id=resmapId)
  resMap <- parseRDF(resMap, rdf=rawToChar(resMapBytes), asText=TRUE)
  # All identifiers from the package are needed for obtaining the triples, including
  # the identifiers for the metadata object and the resource map itself.
  allIds <- packageMembers
  allIds[[length(allIds)+1]] <- resmapId
  allIds[[length(allIds)+1]] <- metadataPid
  relations <- getTriples(resMap, filter=TRUE, identifiers=allIds)
  freeResourceMap(resMap)
  
  if(nrow(relations) > 0) {
    for(irel in 1:nrow(relations)) {
      dpkg <- insertRelationship(dpkg, subjectID=relations[irel, 'subject'],
                                 objectIDs=relations[irel, 'object'],
                                 predicate=relations[irel, 'predicate'],
                                 subjectType=relations[irel, 'subjectType'],
                                 objectType=relations[irel, 'objectType'],
                                 dataTypeURI=relations[irel, 'dataTypeURI'])
    }
  }
  
  # Reset the 'update' status flag on the relations (resourceMap) as this downloaded package
  # has not been updated by the user (after download).
  dpkg@relations[['updated']] <- FALSE 
  
  return(dpkg)
})

#' A method to query the DataONE solr endpoint of the Coordinating Node.
#' @description It expects any lucene reserved characters to already be escaped with backslash. If
#' solrQuery is a list, it is expected to have field names as attributes and search
#' values as the values in the list.
#' @param x  the D1Client (environment) being queried
#' @param solrQuery list or character: a fully encoded query string 
#' @return the solr response (XML)
#' @rdname d1SolrQuery
#' @aliases d1SolrQuery
#' @export
#' @examples \dontrun{ 
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' queryParams <- list(q="id:doi*", rows="5", 
#'     fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", 
#'     fl="title,id,abstract,size,dateUploaded,attributeName")
#' result <- d1SolrQuery(d1c, queryParams)
#' }
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
setGeneric("d1SolrQuery", function(x, solrQuery) { 
  .Deprecated("query", "dataone")
    standardGeneric("d1SolrQuery")
})

#' @rdname d1SolrQuery
setMethod("d1SolrQuery", signature("D1Client", "list"), function(x, solrQuery) {
  result <- query(x@cn, solrQuery, encode=TRUE, as="xml", parse=TRUE)
  return(result)
})

#' @rdname d1SolrQuery
setMethod("d1SolrQuery", signature("D1Client", "character"), function(x, solrQuery) {
  result <- query(x@cn, solrQuery, encode=TRUE, as="xml", parse=TRUE)
  return(result)
})

#' Query the DataONE Solr endpoint of the Coordinating Node.
#' @description The DataONE CN Solr query engine is searched using
#' the provided query string. 
#' @param x  D1Client: representing the DataONE environment being queried
#' @param ... Additional parameters
#' @param solrQuery  character: a query string 
#' @return a vector of identifiers found
#' @rdname d1IdentifierSearch
#' @aliases d1IdentifierSearch
#' @examples \dontrun{
#' library(dataone)
#' client <- new("D1Client")
#' result <- d1IdentifierSearch(client,solrQuery="species population diversity")
#' }
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
setGeneric("d1IdentifierSearch", function(x, ...) {
  .Deprecated("query", "dataone")
    standardGeneric("d1IdentifierSearch")    
})

#' @rdname d1IdentifierSearch
#' @export
setMethod("d1IdentifierSearch", signature("D1Client"), function(x, solrQuery) {
  # TODO: Check if this is still true: Empirical testing shows that prepending the 'fl' and 'wt' fields effectively 
  # negates any other fl or wr values that might be part of the passed in solrQuery
  # (need to do this for parsing the reponse)
  finalQuery <- sprintf("%s&fl=identifier", solrQuery)
  result <- query(x@cn, solrQuery=finalQuery, encode=TRUE, as="data.frame", parse=TRUE)
  ids <- result[,'identifier']
  return(ids)
})

#' @rdname reserveIdentifier
#' @export
setMethod("reserveIdentifier", signature("D1Client"), function(x, id) {
  reserveIdentifier(x@cn, id)
  return(TRUE)
})

#' Create a DataPackage on a DataONE Member Node
#' @description Upload all members of a DataPackage to DataONE.
#' @param x A D1Client instance.
#' @param dataPackage The DataPackage instance to be submitted to DataONE for creation.
#' @param ... Additional arguments
#' @rdname createDataPackage
#' @aliases createDataPackage
#' @return The identifier of the uploaded package.
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' library(dataone)
#' testdf <- data.frame(x=1:10,y=11:20)
#' csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
#' write.csv(testdf, csvfile, row.names=FALSE)
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' dp <- new("DataPackage")
#' emlFile <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
#' emlChar <- readLines(emlFile)
#' emlRaw <- charToRaw(paste(emlChar, collapse="\n"))
#' emlId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' metadataObj <- new("D1Object", id=emlId, format="eml://ecoinformatics.org/eml-2.1.1", data=emlRaw, 
#'   mnNodeId=getMNodeId(d1c))
#' sdf <- read.csv(csvfile)
#' stf <- charToRaw(convert.csv(d1c, sdf))
#' sciId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' sciObj <- new("D1Object", id=sciId, format="text/csv", data=stf, mnNodeId=getMNodeId(d1c))
#' dp <- addMember(dp, do=sciObj, mo=metadataObj)
#' expect_true(is.element(sciObj@dataObject@sysmeta@identifier, getIdentifiers(dp)))
#' resourceMapId <- createDataPackage(d1c, dp, replicate=TRUE, public=TRUE)
#' }
setGeneric("createDataPackage", function(x, dataPackage, ...) { 
  .Deprecated("uploadDataPackage", "dataone")
    standardGeneric("createDataPackage")
})

#' @export
#' @rdname createDataPackage
setMethod("createDataPackage", signature("D1Client", "DataPackage"), function(x, dataPackage, ...) {
  # createDataPackage has been superceded by uploadDataPackage
  uploadDataPackage(x, dataPackage, ...)
})

#########################################################
### Accessor methods
#########################################################

#' Return the URL endpoint for the DataONE Coordinating Node.
#' @description A D1Client object is associated with a DataONE Coordinating Node. This
#' CN is either the production CN (from the "PROD" environment, the default), or a CN from one
#' of the development environments ("STAGING", "SANDBOX", "DEV"). The base URL for the CN
#' is returned.
#' @param x A D1Client object
#' @param ... (Not yet used.)
#' @return A character vector containing the URL of the Coordinating Node
#' @rdname getEndpoint
#' @aliases getEndpoint
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' cli <- D1Client("STAGING2", "urn:node:mnTestKNB")
#' cnUrl <- getEndpoint(cli)
#' }
setGeneric("getEndpoint", function(x, ...) { standardGeneric("getEndpoint")} )

#' @rdname getEndpoint
#' @export
setMethod("getEndpoint", "D1Client", function(x) {
    res <- x@cn@baseURL
    return(res)
})

#' Get the member node identifier associated with this D1Client object..
#' @description One Member Node can be associated with the client as the default to which
#' data and metadata are written.
#' @param x A D1Client object.
#' @return The Member Node identifier  as a character vector
#' @rdname getMNodeId
#' @aliases getMNodeId
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' cli <- D1Client("STAGING2", "urn:node:mnTestKNB")
#' mn <- getMNodeId(cli)
#' }
setGeneric("getMNodeId", function(x) { 
    standardGeneric("getMNodeId")
})

#' @rdname getMNodeId
#' @export
setMethod("getMNodeId", signature("D1Client"), function(x) {
   if(nchar(x@mn@endpoint) == 0) {
     message("Error getMNodeId: This D1Client has not been initialized with a member node value.")
     return(as.character(NA))
   } else {
     return(x@mn@identifier)
   }
})

#' Set the member node identifier to be associated with the D1Client object.
#' @description The member node identifier is the URN identifier used by
#' DataONE to uniquely identifier a node, for example "urn:node:KNB" specifies
#' the "Knowledge Network for Biodiversity" member node.
#' @details One Member Node can be associated with the client as the default to which
#' data and metadata are written.
#' @param x A D1Client object.
#' @param id A DataONE member node identifier.
#' @rdname setMNodeId
#' @author setMNodeId
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
setGeneric("setMNodeId", function(x, id) { 
    standardGeneric("setMNodeId")
})

#' @rdname setMNodeId
#' @export
setMethod("setMNodeId", signature("D1Client", "character"), function(x, id) {
    if(!is.null(id) && id != "") {
        newMN <- getMNode(x@cn, id)
        if (!is.null(newMN)) {
            x@mn <- newMN   
        } else {
            message(paste0("Member Node not found: ", id))
        }
    }
  return(x)
})

#' Get a member node client based on its node identifier.
#' @param x A D1Client object.
#' @param nodeid The identifier of the node to retrieve.
#' @param ... (Not yet used)
#' @rdname getMN
#' @aliases getMN
#' @note This method has been superceded by \code{\link{getMNodeId}}
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' cli <- D1Client("STAGING2", "urn:node:mnTestKNB")
#' testMN <- getMN(cli)
#' }
setGeneric("getMN", function(x, nodeid, ...) { 
    .Deprecated("getMNodeId", "dataone", "getMN(x, nodeid) is deprecated and no longer returns a Java object, use getMNode() instead.", "getMN(x, nodeid")
    standardGeneric("getMN")
})

#' @rdname getMN
#' @export
setMethod("getMN", signature("D1Client"), function(x, ...) {
    return(x@mn)
})

#' @rdname getMN
setMethod("getMN", signature("D1Client", "character"), function(x, nodeid) {
    if(!is.null(nodeid) && (!nodeid == "")) {
        newMN <- getMNode(x@cn, nodeid)
        if (is.null(newMN)) {
            message(paste0("Member Node not found: ", nodeid))
        }
    } else {
      newMN <- as.character(NA)
    }
    return(newMN)
})

#' Get the coordinating node associated with this D1Client object.
#' @param x A D1Client object.
#' @rdname getCN
#' @aliases getCN
#' @note The method getCN has been deprecated.
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' cli <- D1Client("STAGING2", "urn:node:mnTestKNB")
#' testCN <- getCN(cli)
#' }
setGeneric("getCN", function(x) { 
    .Deprecated("getCN(x)", "dataone", "The getCN(x) function no longer returns a Java object. This new function returns a CNode object", "getMN(x, ...)")
    standardGeneric("getCN")
})

#' @rdname getCN
#' @export
setMethod("getCN", signature("D1Client"), function(x) {
    return(x@cn)
})

#' Upload a DataPackage to a DataONE member node.
#' @description Upload all DataObjects contained in the DataPackage by calling \code{\link{uploadDataObject}}
#' on each of the members. Also a resourceMap object is created from the
#' recorded relationships between DataObjects, and this is uploaded as well.
#' @details The DataPackage describes the collection of data object and their associated 
#' metadata object, with the relationships and members serialized into a document
#' stored under, and retrievable with, the packageId as it's own distinct object.
#' Any objects in the data map that have a dataUploaded value are assumed to be 
#' pre-existing in the system, and skipped.
#' @note Member objects are created serially, and most errors in creating one object will 
#' interrupt the create process for the whole, with the result that some members will 
#' be created, and the remainder not.
#' @param x A D1Client instance.
#' @param ... (Not yet used.)
#' @return id The identifier of the resource map for this data package
#' @rdname uploadDataPackage
#' @aliases uploadDataPackage
#' @import datapack
#' @import uuid
#' @export
#' @examples \dontrun{
#' library(dataone)
#' library(datapack)
#' dp <- new("DataPackage")
#' sampleData <- system.file("extdata/sample.csv", package="dataone")
#' dataObj <- new("DataObject", format="text/csv", file=sampleData)
#' dataObj <- setPublicAccess(dataObj)
#' sampleEML <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
#' metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", file=sampleEML)
#' metadataObj <- setPublicAccess(metadataObj)
#' dp <- addMember(dp, do = dataObj, mo = metadataObj)
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' # Upload all members of the DataPackage to DataONE (requires authentication)
#' packageId <- uploadDataPackage(d1c, dp, replicate=TRUE, public=TRUE, numberReplicas=2)
#' }
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
setGeneric("uploadDataPackage", function(x, ...) {
  standardGeneric("uploadDataPackage")
})

#' @rdname uploadDataPackage
#' @param dp The DataPackage instance to be submitted to DataONE for creation.
#' @param replicate A value of type \code{"logical"}, if TRUE then DataONE will replicate this object to other member nodes
#' @param numberReplicas A value of type \code{"numeric"}, for number of supported replicas.
#' @param preferredNodes A list of \code{"character"}, each of which is the node identifier for a node to which a replica should be sent.
#' @param public A \code{'logical'}, if TRUE then all objects in this package will be accessible by any user
#' @param accessRules Access rules of \code{'data.frame'} that will be added to the access policy of each object in the datapackage.
#' @param quiet A \code{'logical'}. If TRUE (the default) then informational messages will not be printed.
#' @param resolveURI A URI to prepend to identifiers (i.e. for use when creating the ResourceMap). See \link[datapack]{serializePackage}
#' @param packageId A value of type \code{"character"} specifying a unique identifier to use for the uploaded package (resource map pid)
#' @param as A value of type \code{"character"} that specifies the return value. Possible values are \code{"character"} (the default) or \code{"DataPackage"}.
#' @importFrom utils flush.console
#' @export
setMethod("uploadDataPackage", signature("D1Client"), function(x, dp, replicate=NA, numberReplicas=NA, preferredNodes=NA,  public=as.logical(FALSE), 
                                                                           accessRules=NA, quiet=as.logical(TRUE), 
                                                                           resolveURI=as.character(NA), packageId=as.character(NA), 
                                                                           as="character", ...) {
  
    # Define the creator that will appear in the ORE Resource Map for this package.
    creator <- "DataONE R Client"
    stopifnot(class(dp) == "DataPackage")
    if (nchar(x@mn@identifier) == 0) {
      stop("Please set the DataONE Member Node to upload to using setMNodeId()")
    }
    
    # First check if any members of this DataPackage have been previously uploaded. If a package
    # was first downloaded from a repository and then at least one member was modified, then this 
    # is considered a package update.
    downloadedPkg <- FALSE
    dates <- getValue(dp, name="sysmeta@dateUploaded")
    for (thisDate in dates) {
        if(!is.na(thisDate)) downloadedPkg <- TRUE
    }
    if(!quiet) {
      if(downloadedPkg) {
          cat(sprintf("Updating a modified package to member node %s\n", x@mn@endpoint))
      } else {
          cat(sprintf("Uploading a new package to member node %s.\n", x@mn@endpoint))
      }
    }
    # Use the CN resolve URI from the D1Client object, if it was not specified on the command line.
    if(is.na(resolveURI)) {
        resolveURI <- paste0(x@cn@endpoint, "/resolve")
    } 
    # Ensure that the resmap has the same permissions as the package members, so
    # create an access policy for the resmap that will have the same APs as the
    # package members.
    resMapAP <- data.frame(subject=as.character(), permission=as.character(), row.names = NULL,
                           stringsAsFactors = FALSE)
    submitter <- as.character(NA)
    uploadedMember <- FALSE
    # Upload each object that has been added to the DataPackage
    for (doId in getIdentifiers(dp)) {
        do <- getMember(dp, doId)
        submitter <- do@sysmeta@submitter
        if (public) {
            do <- setPublicAccess(do)
        }
        
        if(!is.na(do@filename)) {
          fn <- basename(do@filename)
        } else {
          fn <- as.character(NA)
        }
        
        # Add this package member's access policy to the resmap AP
        resMapAP <- rbind(resMapAP, do@sysmeta@accessPolicy)
        
        # If this DataObject has never been uploaded before, then upload it now.
        if(is.na(do@sysmeta@dateUploaded)) {
           returnId <- uploadDataObject(x, do, replicate, numberReplicas, preferredNodes, public, accessRules, quiet=quiet)
            if(is.na(returnId)) {
               warning(sprintf("Error uploading data object with id: %s", getIdentifier(do)))
            } else {
                if(!quiet) cat(sprintf("Uploaded data object with id: %s, filename: %s, size: %s bytes\n", 
                            doId, fn, format(do@sysmeta@size, scientific=FALSE)))
                # Reinsert the DataObject with a SystemMetadata containing the current date as the dateUploaded
                dp <- setValue(dp, name="sysmeta@dateUploaded", value = datapack:::defaultUTCDate(), 
                               identifiers=getIdentifier(do))
                removeMember(dp, doId, removeRelationships=FALSE)
                dp <- addMember(dp, do)
                uploadedMember <- TRUE
            }
        } else {
            # This is not a new DataObject, it may have been downloaded from a repository and modified. 
            # The updateDataObject() method will check if the object has been modified.
            # Check if the user has provided an identifier to use for the updated object. If the object
            # has changed and no identifier is provided, then a new one will be generated.
            # Update the object to the member node
            pid <- getIdentifier(do)
            updateId <- uploadDataObject(x, do=do, replicate=replicate, numberReplicas=numberReplicas,
                                         preferredNodes=preferredNodes, public=public, accessRules=accessRules,
                                         quiet=quiet)
            
            if(!is.na(updateId)) {
                # Reinsert the DataObject with a SystemMetadata containing the current date as the dateUploaded
                # The DataPackage is not returned from this method so these updates won't all persist (except
                # for fields that are hashes(), but leave these lines in here in case we ever decide to return the
                # DataPackage object instead of just the id.
                now <- format.POSIXct(Sys.time(), format="%FT%H:%M:%SZ", tz="GMT", usetz=FALSE)
                dp <- setValue(dp, name="sysmeta@dateUploaded", value = now, identifiers=getIdentifier(do))
                dp <- setValue(dp, name="sysmeta@dateSysMetadataModified", value = now, identifiers=getIdentifier(do))
                # Change the updated status so that this DataObject won't be accidentially uploaded again, if the
                # user calls this function again without actually updated the object..
                dp <- setValue(dp, name="updated[['sysmeta']]", value=FALSE, identifiers=getIdentifier(do))
                dp <- setValue(dp, name="updated[['data']]", value=FALSE, identifiers=getIdentifier(do))
                # Replace the updated member in the DataPackage
                dp <- removeMember(dp, doId, removeRelationships=FALSE)
                dp <- addMember(dp, do)
                # Now update the package relationships, substituting the old id for the new
                dp <- updateRelationships(dp, pid, updateId)
                if(!quiet) cat(sprintf("Updated data object with id: %s, obsoleting id: %s\n", updateId, do@oldId))
            } 
        }
    }
    
    # Now upload or update the resource map if necessary.
    returnId <- as.character(NA)
    # This is a new package, so potentially we need to upload a resource map
    if(!downloadedPkg) {
        # Only upplad a resource map if a DataObjects was uploaded, i.e. not all uploads failed.
        if (uploadedMember) {
            if(!is.na(packageId)) {
                newPid <- packageId
            } else {
                # Construct a resource map pid of the form "resourceMap_<metadata pid>"
                # First get the pid of the metadata object
                metadataId <- getMetadataMember(x, dp)
                if(is.na(metadataId)) {
                    newPid <- sprintf("resource_map_%s", paste0("urn:uuid:", UUIDgenerate()))
                } else {
                    newPid <- sprintf("resource_map_%s", metadataId)
                }
            }
            # Get the pid of the metadata object, if one is available.
            # Remove ':' from filename if on windows (only the pid might have these.)
            # Just use a random id for the filename
            #if(.Platform$OS.type == "windows") {
            #    tf <- tempfile(pattern=sprintf("%s.rdf", gsub(':', '_', newPid)))
            #} else {
            #    tf <- tempfile(pattern=sprintf("%s.rdf", newPid))
            #}
            # Just use a random id for the filename
            tf <- tempfile(pattern=sprintf("%s.rdf", UUIDgenerate()))
            status <- serializePackage(dp, file=tf, id=newPid, resolveURI=resolveURI, creator=creator)
            # Recreate the old resource map, so that it can be updated with a new pid
            resMapObj <- new("DataObject", id=newPid, format="http://www.openarchives.org/ore/terms", filename=tf)
            resMapObj@sysmeta@accessPolicy <- unique(resMapAP)
            returnId <- uploadDataObject(x, resMapObj, replicate, numberReplicas, preferredNodes, public, accessRules,
                                         quiet=quiet)
            
            if(!is.na(returnId)) {
                if(!quiet) cat(sprintf("Uploaded resource map with id: %s\n", returnId))
                dp@resmapId <- returnId
                dp@relations[['update']] <- FALSE
            } else {
                cat(sprintf("Error uploading resource map %s.", getIdentifier(resMapObj)))
            }
        } else {
            if(!quiet) cat(sprintf("No DataObjects uploaded from the DataPackage, so a resource map will not be created and uploaded.\n"))
        }
    } else {
        # This is a package update, so we will update the existing resource map.
        # Because we are updating the package, we must know what the previous resource map pid was,
        # which is stored in the 'resmapId' slot.
        if(is.na(dp@resmapId)) {
            stop("A resource map identifier has not been assigned to the current DataPackage, unable to update the package.")
        }
        # Update the resource map if new package relationships have been added or modified.
        # Use the serial version for the resource map so that updates to the resource map
        # can the format "resource_map_<serial_version>_<metadata pid>
        if(dp@relations[['updated']]) {
            if(!is.na(packageId)) {
                newPid <- packageId
            } else {
                metadataId <- getMetadataMember(x, dp)
                # The metadata pid should be defined, but if it couldn't be determined, then fallback
                # to using just a UUID
                if(is.na(metadataId)) {
                    newPid <- sprintf("resource_map_%s", paste0("urn:uuid:", UUIDgenerate()))
                } else {
                    # Does the resource map pid contain the metadata pid? If no, then set the new
                    # pid to be of the form "resource_map_<metadata pid>"
                    if(!grepl(metadataId, dp@resmapId, fixed=TRUE)) {
                        newPid <- sprintf("resource_map_%s", metadataId)
                    } else {
                        # See if the previous resource map pid is of the form
                        #  "<something>_<revision-number>_<metadata pid> e.g. "resource_map_2_doi:10.18739/A2448T"
                        regex <- sprintf(".*(_([0-9]*)_)%s", metadataId)
                        ver <- str_match(dp@resmapId, regex)[,3]
                        # If the resource map string contains the metadata id, then either add or increment
                        # a version number preceding it. Otherwise, start using the format 'resource_map_<metadaaId>
                        if(is.na(ver)) {
                            newPid <- str_replace(dp@resmapId, metadataId, sprintf("1_%s", metadataId))
                        } else {
                            verMetaId <- sprintf("%s%s", str_match(dp@resmapId, regex)[,2], metadataId)
                            newPid <- str_replace(dp@resmapId, verMetaId, sprintf("_%s_%s", as.numeric(ver)+1, metadataId))
                        }
                    }   
                }
            }
            # Remove ':' from filename if on windows (only the pid might have these.)
            if(.Platform$OS.type == "windows") {
                cleanedPid <- gsub(':', '_', newPid)
                cleanedPid <- gsub('/', '_', cleanedPid)
                tf <- sprintf("%s/%s.rdf", tempdir(), cleanedPid)
            } else {
                cleanedPid <- gsub('/', '_', newPid)
                tf <- sprintf("%s/%s.rdf", tempdir(), cleanedPid)
            }
            status <- serializePackage(dp, file=tf, id=newPid, resolveURI=resolveURI, creator=creator)
            
            # Create a new resource map that will replace the old one.
            resMapObj <- new("DataObject", id=newPid, format="http://www.openarchives.org/ore/terms", filename=tf)
            # Assign the old resource map id (possibly from a repository) for the pid to update
            resMapObj@oldId <- dp@resmapId
            # Make it appear that this object was downloaded and is now being updated. The resource map
            # is different than any other object in the package, because there is not a DataObject contained
            # in the DataPackage, instead the resource map info is stored in the package relationships.
            resMapObj@sysmeta@dateUploaded <- format.POSIXct(Sys.time(), format="%FT%H:%M:%SZ", tz="GMT", usetz=FALSE)
            resMapObj@sysmeta@accessPolicy <- unique(resMapAP)
            resMapObj@updated[['sysmeta']] <- TRUE
            resMapObj@updated[['data']] <- TRUE
            returnId <- uploadDataObject(x, do=resMapObj, replicate=replicate, numberReplicas=numberReplicas, 
                                         preferredNodes=preferredNodes, public=public, accessRules=accessRules,
                                         quiet=quiet) 
            dp@relations[['update']] <- FALSE
            if(!is.na(returnId)) {
                if(!quiet) cat(sprintf("Updated resource map wth new id: %s, obsoleting id: %s\n", newPid, resMapObj@oldId))
                dp@resmapId <- returnId
            } else {
                cat(sprintf("Error updating resource map %s.", resMapObj@oldId))
            }
        } else {
            if(!quiet) cat(sprintf("Package relationships have not been updated so the resource map was not updated"))
        } 
    }
    
    if(as == "character") {
        return(returnId)
    } else if (as == "DataPackage") {
        return(dp)
    } else {
        message(sprintf("Invalid value \"%s\" for argument \"as\", will return \"character\"", as))
        return(returnId)
    }
})

#' Upload a DataObject to a DataONE member node.
#' @param x A D1Client instance. 
#' @param ... (Not yet used.) 
#' @return id The id of the DataObject that was uploaded
#' @rdname uploadDataObject
#' @aliases uploadDataObject
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @import datapack
#' @export
#' @examples \dontrun{
#' library(dataone)
#' library(datapack)
#' testdf <- data.frame(x=1:10,y=11:20)
#' csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
#' write.csv(testdf, csvfile, row.names=FALSE)
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' do <- new("DataObject", format="text/csv", mnNodeId=getMNodeId(d1c), filename=csvfile)
#' # Upload a single DataObject to DataONE (requires authentication)
#' newId <- uploadDataObject(d1c, do, replicate=FALSE, preferredNodes=NA ,  public=TRUE)
#' }
setGeneric("uploadDataObject", function(x, ...) {
    standardGeneric("uploadDataObject")
})

#' @rdname uploadDataObject
#' @param do The DataObject instance to be uploaded to DataONE.
#' @param replicate A value of type \code{"logical"}, if TRUE then DataONE will replicate this object to other member nodes
#' @param numberReplicas A value of type \code{"numeric"}, for number of supported replicas.
#' @param preferredNodes A list of \code{"character"}, each of which is the node identifier for a node to which a replica should be sent.
#' @param public A \code{"logical"} value - if TRUE then the uploaded object will be publicly readable.
#' @param accessRules Access rules of \code{'data.frame'} that will be added to the access policy
#' @param quiet A \code{'logical'}. If TRUE (the default) then informational messages will not be printed.
#' @export
setMethod("uploadDataObject", signature("D1Client"),  function(x, do, replicate=as.logical(FALSE),  numberReplicas=NA,  
                                                               preferredNodes=NA,  public=as.logical(FALSE),  accessRules=NA, 
                                                               quiet=TRUE, ...)  { 
    
    stopifnot(class(do) == "DataObject")
    if (nchar(x@mn@identifier) == 0) {
        stop("Please set the DataONE Member Node using setMNodeId()")
    }
    
    # Checks are made for a DataObject being "new" or "downloaded, not modified" or "downloaded,"
    
    # Object is new, as it has never been uploaded
    if(is.na(do@sysmeta@dateUploaded)) {
        if (nchar(x@mn@identifier) == 0) {
            stop("Please set the DataONE Member Node to upload to using setMN()")
        }
        
        doId <- do@sysmeta@identifier
        # Set sysmeta values if passed in and not already set in sysmeta for each data object
        if (!is.na(replicate)) {
            do@sysmeta@replicationAllowed <- as.logical(replicate)
        }
        if (!is.na(numberReplicas)) {
            do@sysmeta@numberReplicas <- as.numeric(numberReplicas)
        }
        if (!all(is.na(preferredNodes))) {
            do@sysmeta@preferredNodes <- as.list(preferredNodes)
        }
        
        if (public) {
            do@sysmeta <- addAccessRule(do@sysmeta, "public", "read")
        }
        
        # addAccessRule will add all rules (rows) in accessRules in one call
        if(!all(is.na(accessRules))) {
            do@sysmeta <- addAccessRule(do@sysmeta, accessRules)
        }
        
        if (!is.na(do@sysmeta@dateUploaded)) {
            msg <- sprintf("SystemMetadata indicates that the object with pid: %s was already uploaded to DataONE on %s.\n", do@sysmeta@identifier, do@sysmeta@dateUploaded)
            msg <- sprintf("%sThis object will not be uploaded.", msg)
            warning(msg)
            # options(warn) may be set to essentially ignore warnings, so return NA if this is the case.
            return(as.character(NA))
        }
        
        # If the DataObject has both @filename and @data defined, filename takes precedence 
        if(!is.na(do@filename)) {
            # Upload the data to the MN using create(), checking for success and a returned identifier
            createdId <- createObject(x@mn, doId, do@filename, do@sysmeta)
        } else {
            if(length(do@data == 0)) {
                # Write the DataObject raw data to disk and upload the resulting file.
                tf <- tempfile()
                con <- file(tf, "wb")
                writeBin(do@data, con)
                close(con)
                createdId <- createObject(x@mn, doId, tf, do@sysmeta)
                file.remove(tf)
            } else {
                warning(sprintf("DataObject %s cannot be uploaded, as neither @filename nor @data slots are set.", do@sysmeta@identifier))
            }
        }
        
        if (is.null(createdId)) {
            return(as.character(NA))
        } else {
            return(createdId)
        }
    } else {
        # This object has been downloaded from a repository and possibly updated.
        if(!is.na(do@sysmeta@obsoletedBy)) {
            msg <- sprintf("This DataObject with identifier %s has been obsoleted by identifier %s\nso will not be updated", 
                           do@sysmeta@identifier, do@sysmeta@obsoletedBy)
            cat(sprintf(msg))
            return(as.character(NA))
        }
        
        pid <- getIdentifier(do)
        # Update the sysmeta with the necessary new values
        # Set these values to NA so they won't be included in the serialized sysmeta,
        # as DataONE will complain or get confused if they are set. DataONE will
        # set these values on upload/update.
        do@sysmeta@obsoletes <- as.character(NA)
        do@sysmeta@obsoletedBy <- as.character(NA)
        do@sysmeta@archived <- as.logical(NA)
        # Set sysmeta values if passed in and not already set in sysmeta for each data object
        if (!is.na(replicate)) {
            do@sysmeta@replicationAllowed <- as.logical(replicate)
        }
        if (!is.na(numberReplicas)) {
            do@sysmeta@numberReplicas <- as.numeric(numberReplicas)
        }
        if (!all(is.na(preferredNodes))) {
            do@sysmeta@preferredNodes <- as.list(preferredNodes)
        }
        if (public) {
            do@sysmeta <- addAccessRule(do@sysmeta, "public", "read")
        }
        # addAccessRule will add all rules (rows) in accessRules in one call
        if (!all(is.na(accessRules))) {
            do@sysmeta <- addAccessRule(do@sysmeta, accessRules)
        }
        
        # Check if this object has been updated. If neither the sysmeta or the data have
        # not been updated, then we can skip it.
        updateId <- as.character(NA)
        if(!do@updated[['sysmeta']] && !do@updated[['data']]) {
            if(!quiet) sprintf("Neither the system metadata nor the data has changed for DataObject %s, so it will not updated.", pid)
        } else if(do@updated[['sysmeta']] && !do@updated[['data']]) {
            # Just update the sysmeta, as it changed, but the data did not.
            updated <- updateSystemMetadata(x@mn, pid=pid, sysmeta=do@sysmeta)
            if(!quiet) cat(sprintf("Updated sysmetadata for DataObject %s.", pid))
            updateId <- pid
        } else {
            oldId <- do@oldId
            # The obsoleting object will always have serialVersion = 1, it's new!
            do@sysmeta@serialVersion <- 1
            if(is.na(oldId)) {
                stop(sprintf("DataObject for id %s does not have a previous pid defined.\n", pid))
            }
            if(oldId == pid) {
                stop("The identifier of the existing DataObject is the same as the previous pid (the pid before it was modified).\n")
            }
            # Both sysmeta and data have changed, so update them both. (The case of data being updated and not sysmeta isn't possible)
            # If the DataObject has both @filename and @data defined, filename takes precedence
            if (!is.na(do@filename)) {
                # Upload the data to the MN using updateObject(), checking for success and a returned identifier
                updateId <- updateObject(x@mn, pid=oldId, file=do@filename, newpid=pid, sysmeta=do@sysmeta)
            } else {
                if (length(do@data != 0)) {
                    # Write the DataObject raw data to disk and upload the resulting file.
                    tf <- tempfile()
                    con <- file(tf, "wb")
                    writeBin(do@data, con)
                    close(con)
                    updateId <- updateObject(x@mn, pid=oldId, file=tf, newpid=pid, sysmeta=do@sysmeta)
                    file.remove(tf)
                } else {
                    warning(
                        sprintf("DataObject %s cannot be uploaded, as neither @filename nor @data are set.", pid)
                    )
                }
            }
            if(!quiet) {
                if(is.na(updateId)) {
                    cat(sprintf("Unable to upload data object with new id %s to replace id: %s.\n", pid, oldId))
                } else {
                    sprintf("Uploaded data object with new id: %s, obsoleting id: %s.", pid, oldId)
                }
            }
        }
        return(updateId) 
    }
})

#' List DataONE Member Nodes.
#' @description A D1Client object is associated with a DataONE Coordinating Node. The
#' \code{listMemberNodes} method lists all member nodes associated with a CN.
#' @param x A D1Client object.
#' @rdname listMemberNodes
#' @aliases listMemberNodes
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' d1c <- D1Client("PROD")
#' nodelist <- listMemberNodes(d1c)
#' }
setGeneric("listMemberNodes", function(x) {
    standardGeneric("listMemberNodes")
})

#' @rdname listMemberNodes
#' @export
setMethod("listMemberNodes", signature("D1Client"), function(x) {
    return (listNodes(x@cn))
})

#########################################################
### Utility methods
#########################################################

#' Convert a DataFrame to Standard CSV.
#' @param x A D1Client object
#' @param df the dataFrame
#' @param ... additional params passed to write.csv
#' @rdname convert.csv
#' @aliases convert.csv
#' @return the dataframe serialized as a .csv
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @importFrom utils write.csv
#' @examples \dontrun{ 
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' testdf <- data.frame(x=1:10,y=11:20)
#' sdf <- convert.csv(d1c, testdf)  
#' }
setGeneric("convert.csv", function(x, ...) {
    .Deprecated("write.csv", "utils")
    standardGeneric("convert.csv")
})

#' @rdname convert.csv
#' @export
setMethod("convert.csv", signature(x="D1Client"), function(x, df, ...) {
    con <- textConnection(NULL, "w")
    write.csv(df, file=con, row.names = FALSE, ...)
    csvbuf <- textConnectionValue(con)
    close(con)
    csvdata <- paste(csvbuf, collapse="\n")
    return(csvdata)
})

#' Encode the Input for a URL Query Segment.
#' @description Encodes the characters of the input so they are not interpreted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param x A D1Client object.
#' @param ... (Not yet used.)
#' @rdname encodeUrlQuery
#' @aliases encodeUrlQuery
#' @return the encoded form of the input
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @examples \dontrun{
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' fullyEncodedQuery <- paste0("q=id:",
#'     encodeUrlQuery(d1c, encodeSolr("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5")))
#' }
#' @export
setGeneric("encodeUrlQuery", function(x, ...) {
  standardGeneric("encodeUrlQuery")
})

#' @rdname encodeUrlQuery
#' @param querySegment : a string to encode
#' @export
setMethod("encodeUrlQuery", signature(x="D1Client"), function(x, querySegment, ...) {
    #luceneExample <- "+pool +ABQ\\:Bernalillo \\[NM\\] -sharks \"kids & adults = fun day\"" 
    #luceneReservedCharExample <- "example__\\+_\\-_\\&_\\|_\\!_\\^_\\~_\\*_\\?_\\:_\\\"_\\(_\\)_\\{_\\}_\\[_\\]____"
  stopifnot(is.character(querySegment))
    
    # This only works for ASCII characters. 
    # (may need to check the behavior of {,},[,] - they may need to be hidden also)
    # Lucene special characters: + - && || ! ( ) { } [ ] ^ " ~ * ? : \
    escaped <- gsub("\\\\([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","%5C\\1",querySegment, perl=TRUE)
    escaped <- gsub("%5C&","%5C%26", escaped)   #  need to hide the ampersand from the web server
    escaped <- gsub("%5C\\+","%5C%2B", escaped) # need to hide the + from the web server
    return(escaped)
})

#' Encode the Input for a URL Path Segment.
#' @description Encodes the characters of the input so they are not interpreted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param x A D1Client object
#' @param ... (Not yet used.)
#' @return the encoded form of the input
#' @rdname encodeUrlPath
#' @aliases encodeUrlPath
#' @examples \dontrun{
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' fullyEncodedPath <- paste0("cn/v1/object/", 
#'     encodeUrlPath(d1c, "doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5"))
#' }
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
setGeneric("encodeUrlPath", function(x, ...) {
    standardGeneric("encodeUrlPath")
})

#' @rdname encodeUrlPath
#' @param pathSegment : a string to encode
#' @export
setMethod("encodeUrlPath", signature(x="D1Client"), function(x, pathSegment, ...) {
     return(URLencode(pathSegment))
})

#' Add a D1Object containing a data object to a DataPackage
#' @rdname addData
#' @description The D1Object \code{do} is added to the data package \code{x}.
#' @details If the optional \code{mo} parameter is specified, then it is assumed that this DataObject is a metadata
#' object that describes the data object that is being added. The DataObject specified in the \code{mo} parameter will
#' also be added to the DataPackage, if it has not already been added. Then the \code{addData} function will add a relationship
#' to the resource map that indicates that the metadata object describes the science object, using CiTO, the Citation Typing Ontology, 
#' \code{documents} and \code{isDocumentedBy} relationships.
#' @param x The \code{"DataPackage"} to which the data object should be added.
#' @param do A D1Object to add to the DataPackage
#' @param mo A D1Object (containing metadata describing \code{"do"} ) to associate with the data object.
#' @export
#' @examples \dontrun{
#' library(dataone)
#' library(datapack)
#' library(uuid)
#' dp <- new("DataPackage")
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' # Create metadata object that describes science data
#' newId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' csvfile <- system.file("extdata/sample.csv", package="dataone")
#' sciObj <- new("DataObject", id=newId, format="text/csv",filename=csvfile)
#' dp <- addData(dp, do = sciObj)
#' }
setMethod("addData", signature("DataPackage", "D1Object"), function(x, do, mo=as.character(NA)) {
  
  # Add deprecated here instead of in the generic function, as the generic function is the datapack R package.
  msg <- sprintf("'addData' is deprecated.\nUse 'datapack:addMember' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("addMember(x, do, mo)")
  .Deprecated("DataObject", package="datapack", msg, methodSig)
  x@objects[[do@dataObject@sysmeta@identifier]] <- do@dataObject
  # If a metadata object identifier is specified on the command line, then add the relationship to this package
  # that associates this science object with the metadata object.
  if (!missing(mo)) {
    # CHeck that the metadata object has already been added to the DataPackage. If it has not
    # been added, then add it now.
    if (!containsId(x, getIdentifier(mo@dataObject))) {
      moId <- addMember(x, mo@dataObject)
    }
    # Now add the CITO "documents" and "isDocumentedBy" relationships
    insertRelationship(x, getIdentifier(mo@dataObject), getIdentifier(do@dataObject))
  }
  return(x)
})


#' Get the DataObject containing package metadata
#' @description Each DataObject in the DataPackage is inspected to see if it matches one
#' of the formats supported by DataONE for metadata. If a package member's format matches
#' one of the supported formats, the identifier for that member is returned.
#' @details This method calls the DataONE CN 'format' service to obtain the current format
#' list.
#' @param x A D1Client object
#' @param dp A DataPackage object
#' @param ... (Additional arguments, Not yet used.)
#' @return The identifier of the metadata object
#' @rdname getMetadataMember
#' @aliases getMetadataMember
#' @export
setGeneric("getMetadataMember", function(x, dp, ...) {
    standardGeneric("getMetadataMember")
})

#' @export
#' @rdname getMetadataMember
#' @param as A value of type \code{"character"} that specifies the return value. Possible values are \code{"character"} (the default) or \code{"DataPackage"}.
setMethod("getMetadataMember", signature("D1Client", "DataPackage"), function(x, dp, as="character", ...) {
    formats <- listFormats(x@cn)
    if(is.null(formats) || length(formats) == 0) {
       return(as.character(NA)) 
    }
    for (irow in 1:nrow(formats)) {
        thisFormat <- formats[irow,]
        thisType <- thisFormat$Type
        if(thisType != "METADATA") next
        thisID <- thisFormat$ID
        # Search for this formatId in each of the package members sysmeta
        id <- selectMember(dp, name="sysmeta@formatId", value=thisID)
        # Someday a package might have more than one metadata member,
        # so just return the first one.
        if(length(id) > 0) {
            if(as == "character") {
                return(id[[1]])
            } else if (as == "DataObject") {
                return(getMember(dp, id[[1]]))
            } else {
                message(sprintf("Invalid value \"%s\" for argument \"as\", will return \"character\"", as))
                return(id[[1]])
            }
        }
    }
    
    # Didn't find the metadata format
    return(as.character(NA))
})

#' Download an object from the DataONE Federation to Disk.
#' @description A convenience method to download an object to disk.
#' @details This method performs multiple underlying calls to the DataONE repository network. 
#' CN.resolve() is called to locate the object on one or more repositories, and then each of these
#' is accessed until success at downloading the associated SystemMetadata for the object. 
#' The SystemMetadata is used to assign a name to the file that is output to disk. If a fileName is specified in
#' the SystemMetadata, then the file output to disk will be named according to the SystemMetadata fileName. 
#' If there is not a specified SystemMetadata fileName, the identifier will be used as the file name output to disk.
#' If the indentifier is used as the file name, a file name extesion will be determined using the SystemMetadata
#' formatID along with information from CNCore.listFormats(). If the SystemMetadata formatID is
#' "application/octet-stream" no extension will be written.
#' @param x A D1Client object.
#' @param identifier The identifier of the object to get.
#' @param path (optional) Path to a directory to write object to. The name of the file will be determined from the
#' SystemMetada of the object (see details for more information).
#' The function will fail if a file with the same name already exists in the directory.
#' @param check (optional) A logical value, if TRUE check if this object has been obsoleted by another object in DataONE.
#' @param ... (Not yet used.)
#' @rdname downloadObject
#' @aliases downloadObject
#' @return A path where the ouput file is written to.
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples \dontrun{
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
#' pid <- "solson.5.1"
#' path <- downloadObject(d1c, pid)
#' }
setGeneric("downloadObject", function(x, identifier, ...) { 
  standardGeneric("downloadObject")
})

#' @rdname downloadObject
#' @export
setMethod("downloadObject", "D1Client", function(x, identifier, path = getwd(), check = as.logical(TRUE)) {
  
  stopifnot(is.character(identifier))
  stopifnot(is.character(path))
  if(!dir.exists(path)) {
    stop("path is not a valid directory path")
  }
  
  suppressMessages(result <- resolve(x@cn, identifier))
  if(is.null(result)) {
    # If the object isn't resolved from the CN, then try the member node directly. Fill out the
    # mntable as if the member node had returned the object.
    mntable <- data.frame(nodeIdentifier=x@mn@identifier, 
                          baseURL=x@mn@baseURL,
                          url=sprintf("%s/%s/object/%s", x@mn@baseURL, x@mn@APIversion, 
                                      URLencode(identifier, reserved=TRUE)), 
                          row.names=NULL, stringsAsFactors=FALSE)
  } else {
    mntable <- result$data
  }
  
  # Get the SystemMetadata and dataURL from one of the MNs
  # Process them in order, until we get non-NULL responses from a node
  sysmeta <- NA
  success <- FALSE
  dataURL <- as.character(NA)
  for (i in seq_along(mntable)) { 
    suppressWarnings(currentMN <- getMNode(x@cn, mntable[i,]$nodeIdentifier))
    # If cn couldn't return the member node, then fallback to the D1Client@mn
    if(is.null(currentMN)) currentMN <- x@mn
    if (!is.null(currentMN)) {
      sysmeta <- getSystemMetadata(currentMN, identifier)
      success <- TRUE
      dataURL <- mntable[i,]$url
      break
    }
  }
  
  if(!success) {
    message(sprintf("Unable to download object with identifier: %s\n", identifier))
    return(NULL)
  }
  
  # Check if the requested identifier has been obsoleted by a newer version
  # and print a warning
  if (check) {
    if (!is.na(sysmeta@obsoletedBy)) {
      message(sprintf('Warning: identifier "%s" is obsoleted by identifier "%s"', identifier, sysmeta@obsoletedBy))
    }
  }
  
  # Get filename if it exists in sysmeta
  if (!is.na(sysmeta@fileName)) {
    fileName <- sysmeta@fileName
    
    # If filename is not in sysmeta use the identifier with an extension determined by the formatID
  } else {
    fileName <- gsub("[^a-zA-Z0-9\\.\\-]+", "_", identifier)
    
    if (sysmeta@formatId != "application/octet-stream") {
      formatIDs <- listFormats(x@cn)
      Extension <- formatIDs$Extension[which(formatIDs$ID == sysmeta@formatId)]
      fileName <- paste0(fileName, ".", Extension)
    }
    
  }
  
  path <- file.path(path, fileName)
  response <- auth_get(dataURL, node = currentMN, path = path)
  
  if (response$status_code != "200") {
    stop(sprintf("get() error: %s\n", getErrorDescription(response)))
  }
  
  return(response$content)
})


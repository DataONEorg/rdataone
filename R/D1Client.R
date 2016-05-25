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
#' @importFrom utils URLencode
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{D1Client}}}{: Construct a D1Client object.}
#'  \item{\code{\link{getD1Object}}}{: Download a data object from the DataONE Federation.}
#'  \item{\code{\link{getDataObject}}}{: Get the data content of a specified data object}
#'  \item{\code{\link{d1SolrQuery}}}{: A method to query the DataONE solr endpoint of the Coordinating Node.}
#'  \item{\code{\link{d1IdentifierSearch}}}{: Query the DataONE Solr endpoint of the Coordinating Node.}
#'  \item{\code{\link{reserveIdentifier}}}{: Reserve a unique identifier in the DataONE Network.}
#'  \item{\code{\link{createDataPackage}}}{: Create a DataPackage on a DataONE Member Node}
#'  \item{\code{\link{getEndpoint}}}{: Return the URL endpoint for the DataONE Coordinating Node}
#'  \item{\code{\link{getMNodeId}}}{: Get the member node identifier associated with this D1Client object.}
#'  \item{\code{\link{getMN}}}{: Get a member node client based on its node identifier.}
#'  \item{\code{\link{uploadDataPackage}}}{: Upload a DataPackage to a DataONE member node.}
#'  \item{\code{\link{uploadDataObject}}}{: Upload a DataObject to a DataONE member node..}
#'  \item{\code{\link{listMemberNodes}}}{: List DataONE Member Nodes.}
#'  \item{\code{\link{convert.csv}}}{: Convert a DataFrame to Standard CSV.}
#'  \item{\code{\link{encodeUrlQuery}}}{: Encode the Input for a URL Query Segment.}
#'  \item{\code{\link{encodeUrlPath}}}{: Encode the Input for a URL Path Segment.}
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @export
#' @import datapack
setClass("D1Client", slots = c(cn = "CNode", mn="MNode"))

#########################
## D1Client constructors
#########################

#' The DataONE client class used to downlaod, update and search for data in the DataONE network.
#' @rdname D1Client
#' @param x The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV')
#' @param y The node Id of the application's 'home' node.  Should be already registered to the corresponding 'env'
#' @param ... (not yet used)
#' @return the D1Client object representing the DataONE environment
#' @seealso \code{\link[=D1Client-class]{D1Client}}{ class description.}
#' @export
#' @examples 
#' cli <- D1Client("PROD", "urn:node:KNB")
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

#' Initialize a D1Client object
#' @param .Object A D1client object.
#' @param cn The Member Node object to associate this D1Client with.
#' @param mn The Member Node object to associate this D1Client with.
#' @param env The DataONE environment to intialize this D1Client with, e.g. "PROD", "STAGING", "SANDBOX", "DEV"
#' @param mNodeid The node identifier of the Member Node to associate with this D1Client.
#' @rdname D1Client-initialize
#' @aliases D1Client-initialize
#' @export
#' @examples 
#' library(dataone)
#' d1c <- D1Client("PROD", "urn:node:KNB")
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
#' data <- readLines(system.file("extdata/sample-eml.xml", package="dataone"))
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
#' @description An objectd is download from the DataONE network for the identifier that is provided.
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

#' Download a data object from the DataONE Federation as a DataObject.
#' @description A convenience method to download a data object and its associated SystemMetadata, wrapped
#' in a DataObject class.
#' @details This method performs multiple underlying calls to the DataONE repository network. 
#' CN.resolve() is called to locate the object on one or more repositories, and then each of these
#' is accessed until success at downloading the associated SystemMetadata and data bytes, which are 
#' finally wrapped in a DataObject and returned. Replaces previous getD1Object() method in the version 1
#' dataone library.
#' @param x A D1Client object.
#' @param identifier The identifier of the object to get.
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
setMethod("getDataObject", "D1Client", function(x, identifier) {
    
    # Resolve the object location
    result <- resolve(x@cn, identifier)
    if(is.null(result)) {
      message("Unable to download object with identifier: %s\n", identifier)
      return(NULL)
    }
    mntable <- result$data
    
    # Get the SystemMetadata and object bytes from one of the MNs
    # Process them in order, until we get non-NULL responses from a node
    sysmeta <- NA
    bytes <- NA
    if(nrow(mntable) > 0) {
      for (i in 1:nrow(mntable)) { 
        suppressWarnings(currentMN <- getMNode(x@cn, mntable[i,]$nodeIdentifier))
        if (!is.null(currentMN)) {
          sysmeta <- getSystemMetadata(currentMN, identifier)
          bytes <- getObject(currentMN, identifier)
          if (!is.null(sysmeta) & !is.null(bytes)) {
            success=TRUE
            break
          }
        }
      }
    } else {
      message("Unable to download object with identifier: %s\n", identifier)
      return(NULL)
    }
    
    # Construct and return a DataObject
    do <- new("DataObject", id=sysmeta@identifier, dataobj=bytes)
    do@sysmeta <- sysmeta
    return(do)
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
#' emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
#' emlChar <- readLines(emlFile)
#' emlRaw <- charToRaw(paste(emlChar, collapse="\n"))
#' emlId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' metadataObj <- new("D1Object", id=emlId, format="eml://ecoinformatics.org/eml-2.1.1", data=emlRaw, 
#'   mnNodeId=getMNodeId(d1c))
#' sdf <- read.csv(csvfile)
#' stf <- charToRaw(convert.csv(d1c, sdf))
#' sciId <- sprintf("urn:uuid:%s", UUIDgenerate())
#' sciObj <- new("D1Object", id=sciId, format="text/csv", data=stf, mnNodeId=getMNodeId(d1c))
#' dp <- addData(dp, do=sciObj, mo=metadataObj)
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
#' @description The membor node identifier is the URN identifier used by
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
#' @param nodeid The identifier of the node to retreive.
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
#' sampleEML <- system.file("extdata/sample-eml.xml", package="dataone")
#' metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", file=sampleEML)
#' metadataObj <- setPublicAccess(metadataObj)
#' dp <- addData(dp, do = dataObj, mo = metadataObj)
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
#' @importFrom utils flush.console
#' @export
setMethod("uploadDataPackage", signature("D1Client"), function(x, dp, replicate=NA, numberReplicas=NA, preferredNodes=NA,  public=as.logical(FALSE), 
                                                                           accessRules=NA, quiet=as.logical(TRUE), 
                                                                           resolveURI=as.character(NA), ...) {
  stopifnot(class(dp) == "DataPackage")
    if (nchar(x@mn@identifier) == 0) {
      stop("Please set the DataONE Member Node to upload to using setMN()")
    }
  
    # Ensure that the resmap has the same permissions as the package members, so
    # create an access policy for the resmap that will have the same APs as the
    # package members.
    resMapAP <- data.frame(subject=as.character(), permission=as.character())
    
    submitter <- as.character(NA)
    # Upload each object that has been added to the DataPackage
    for (doId in getIdentifiers(dp)) {
        do <- getMember(dp, doId)
        submitter <- do@sysmeta@submitter
        if (public) {
            if(!quiet) cat(sprintf("Setting public access for object with id: %s\n", doId))
            do <- setPublicAccess(do)
        }
        
        if(!is.na(do@filename)) {
          fn <- basename(do@filename)
        } else {
          fn <- as.character(NA)
        }
        if(!quiet) {
          cat(sprintf("Uploading data object to %s with id: %s, filename: %s, size: %s bytes\n", 
                               x@mn@endpoint, doId, fn, format(do@sysmeta@size, scientific=FALSE)))
          flush.console()
        }
        returnId <- uploadDataObject(x, do, replicate, numberReplicas, preferredNodes, public, accessRules)
        if(!is.null(returnId)) {
          if(!quiet) cat(sprintf("Uploaded identifier: %s\n", returnId))
          # Reinsert the DataObject with a SystemMetadata containing the current date as the dateUploaded
          do@sysmeta@dateUploaded <- datapack:::defaultUTCDate()
          removeMember(dp, doId)
          addData(dp, do)
          
          # Add this package member's access policy to the resmap AP
          resMapAP <- rbind(resMapAP, do@sysmeta@accessPolicy)
          
        } else {
          warning(sprintf("Error uploading data object with id: %s", getIdentifier(do)))
        }
    }
    
    tf <- tempfile()
    serializationId <- paste0("urn:uuid:", UUIDgenerate())
    status <- serializePackage(dp, file=tf, id=serializationId, resolveURI=resolveURI)
    resMapObj <- new("DataObject", id=serializationId, format="http://www.openarchives.org/ore/terms", user=submitter, mnNodeId=x@mn@identifier, filename=tf)
    resMapObj@sysmeta@accessPolicy <- unique(resMapAP)
    if(!quiet) cat(sprintf("Uploading resource map with id %s to %s\n", getIdentifier(resMapObj), x@mn@endpoint))
    returnId <- uploadDataObject(x, resMapObj, replicate, numberReplicas, preferredNodes, public, accessRules)
    if(!quiet) cat(sprintf("Uploading identifier: %s\n", returnId))
    return(returnId)
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
#' @examples
#' library(dataone)
#' library(datapack)
#' testdf <- data.frame(x=1:10,y=11:20)
#' csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
#' write.csv(testdf, csvfile, row.names=FALSE)
#' d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
#' do <- new("DataObject", format="text/csv", mnNodeId=getMNodeId(d1c), filename=csvfile)
#' # Upload a single DataObject to DataONE (requires authentication)
#' \dontrun{
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
#
#' @export
setMethod("uploadDataObject", signature("D1Client"), 
    function(x, do, replicate=as.logical(FALSE), numberReplicas=NA, 
             preferredNodes=NA, public=as.logical(FALSE), accessRules=NA, ...)  {
      stopifnot(class(do) == "DataObject")
      
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
      return(NULL)
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
        warning(sprintf("DataObject %s cannot be uploaded, as neither @filename nor @data are set.", do@sysmeta@identifier))
      }
    }
    
    #    if (is.null(createdId) | !grepl(newid, xmlValue(xmlRoot(createdId)))) {
    if (is.null(createdId) || doId != createdId) {
        #warning(paste0("Error on returned identifier: ", createdId))
        return(NULL)
    } else {
        return(doId)
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
#' @examples {
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
#' @description Encodes the characters of the input so they are not interpretted as reserved
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
#' @description Encodes the characters of the input so they are not interpretted as reserved
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
  msg <- sprintf("'addData' is deprecated.\nUse 'datapack:addData' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("addData(x, do, mo)")
  .Deprecated("DataObject", package="datapack", msg, methodSig)
  x@objects[[do@dataObject@sysmeta@identifier]] <- do@dataObject
  # If a metadata object identifier is specified on the command line, then add the relationship to this package
  # that associates this science object with the metadata object.
  if (!missing(mo)) {
    # CHeck that the metadata object has already been added to the DataPackage. If it has not
    # been added, then add it now.
    if (!containsId(x, getIdentifier(mo@dataObject))) {
      moId <- addData(x, mo@dataObject)
    }
    # Now add the CITO "documents" and "isDocumentedBy" relationships
    insertRelationship(x, getIdentifier(mo@dataObject), getIdentifier(do@dataObject))
  }
  return(x)
})

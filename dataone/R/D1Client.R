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

#' @include CNode.R
#' @include MNode.R
#' @include D1Object.R

#'
#' @slot endpoint The baseurl of the CN in that environment
#' @slot mn.nodeid The NodeReference for the 'home' MemberNode for this application, where creates/updates will happen.
#' @slot client The reference to the internally held Java D1Client instance
#' @slot session The reference to the internally held Java Session instance
#' @exportClass D1Client
#' @import datapackage
setClass("D1Client", slots = c(cn = "CNode", mn="MNode"))

#########################
## D1Client constructors
#########################

## Generic function with 0, 1, or 2 parameters
## 
## @param env The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV')
## @param mn_nodeid The node Id of the application's 'home' node.  Should be already registered to the corresponding 'env'
## @param ... (not yet used)
## @returnType D1Client  
## @return the D1Client object representing the DataONE environment
## 
## @author mbjones
## @export
setGeneric("D1Client", function(env, mnNodeid, ...) {
    standardGeneric("D1Client")
})

#' Construct a D1Client, using default env ("PROD") and nodeid ("")
#' 
#' @return the D1Client object representing the DataONE environment
#' 
#' @export
setMethod("D1Client", , function() {
    result <- D1Client("PROD", "")
    return(result)
})

#' Pass in the environment to be used by this D1Client, but use
#'   the default member node.
#' @export
setMethod("D1Client", signature("character"), function(env, ...) {
    #message("Instantiating D1Client without a default Member Node.")
    result <- D1Client(env, "")
    return(result)
})

#' Pass in the environment to be used by this D1Client, plus the 
#' id of the member node to be used for primary interactions such as creates
#' @export
setMethod("D1Client", signature("character", "character"), function(env, mnNodeid) {
    
    # create new D1Client object and insert uri endpoint
    result <- new("D1Client")
    result@cn <- CNode(env)
    
    # an expired certificate can crash the R session, so want to check before 
    # instantiating any Java objects, which might interact with the DataONE environment
    # while setting things up.  (It will be called in this routine when 
    # validating the member node id)
    cm <- CertificateManager()
    if (isCertExpired(cm)) {
        message("Your client certificate is expired.  Please download a new one before continuing...")
        return(NULL)
    }
    
    # Check and set the node reference
    if (mnNodeid == "") {
        # allow the mn to be unset with empty string only
        # result@mn <- NULL
    } else {
        # try to instantiate a MN from the node identifier in this CN environment
        mn <- getMNode(result@cn, mNodeid)
    }
    
    return(result)
})

#' @export
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
    if (!missing(mNodeid)) {
        .Object@mn <- getMNode(.Object@cn, mNodeid)
    }
    return(.Object)
})


#########################################################
### MNAuthentication methods
#########################################################


#########################################################
### MNRead methods
#########################################################


#' Download a DataPackage from the DataONE Federation.
#' 
#' Given a valid identifier for a ResourceMap, instantiate an R DataPackage
#' object containing all of the package members. 
#' @param x D1Client
#' @param identifier character: identifier of the ResourceMap 
#' @param ... (not yet used)
#' @return the data package
#'
#' @import datapackage
#' @export
setGeneric("getPackage", function(x, identifier, ...) { 
    standardGeneric("getPackage")
})

setMethod("getPackage", signature("D1Client", "character"), function(x, identifier) {
    
    dp <- DataPackage()
    
    # TODO: Download the ResourceMap from the DataONE CN
    
    # TODO: Deserialize the ResourceMap into a ResourceMap instance
    
    # TODO: Create a new DataPackage instance with info from the ResourceMap
    
    # TODO: Loop over the aggregated package members
        # Download it and add it to the package
        # Add any relationships for this member
    
    # Return the newly constructed DataPackage
    return(dp)
})


#' Download a DataObject from the DataONE Federation. 
#' 
#' @description A convenience method to download a data object and its associated SystemMetadata, wrapped
#' in a DataObject class.
#' @details This method performs multiple underlying calls to the DataONE repository network. 
#' CN.resolve() is called to locate the object on one or more repositories, and then each of these
#' is accessed until success at downloading the associated SystemMetadata and data bytes, which are 
#' finally wrapped in a DataObject and returned. Replaces previous getD1Object() method in the version 1
#' dataone library.
#' @param x : D1Client
#' @param identifier : the identifier of the object to get
#' @param ... (not yet used)
#' @return the DataONE object
#'
#' @export
setGeneric("getDataObject", function(x, identifier, ...) { 
    standardGeneric("getDataObject")
})

#' @export
setMethod("getDataObject", "D1Client", function(x, identifier) {
    
    # Resolve the object location
    result <- resolve(x@cn, identifier)
    mntable <- result[[2]]
    
    # Get the SystemMetadata and object bytes from one of the MNs
    # Process them in order, until we get non-NULL responses from a node
    sysmeta <- NA
    bytes <- NA
    for (i in 1:length(mntable)) { 
        currentMN <- getMNode(x@cn, mntable[i,]$identifier)
        if (!is.null(currentMN)) {
            sysmeta <- getSystemMetadata(currentMN, identifier)
            bytes <- get(currentMN, identifier)
            if (!is.null(sysmeta) & !is.null(bytes)) {
                success=TRUE
                break
            }
        }
    }
    
    # Construct and return a DataObject
    do <- new("DataObject", id=sysmeta, data=bytes)
    return(do)
})

#' A method to query the DataONE solr endpoint of the Coordinating Node.
#' It expects any lucene reserved characters to already be escaped with backslash. If
#' solrQuery is a list, it is expected to have field names as attributes and search
#' values as the values in the list.
#' @param x  the D1Client (environment) being queried
#' @param solrQuery list or character: a fully encoded query string 
#' @return the solr response (XML)
#' @examples \dontrun{ d1SolrQuery(client,list(q="+species population diversity", fl="identifier")) }
#' @export
setGeneric("d1SolrQuery", function(x, solrQuery) { 
    standardGeneric("d1SolrQuery")
})

setMethod("d1SolrQuery", signature("D1Client", "list"), function(x, solrQuery) {
    
#     encodedKVs <- character()
#     for(key in attributes(solrQuery)$names) {
#         kv <- paste0(encodeUrlQuery(x,key), "=", encodeUrlQuery(x,solrQuery[[key]]))
#         if (!is.null(e<-.jgetEx())) {
#             print("Java exception was raised")
#             print(paste("Exception detail code:", e$getDetail_code()))
#             print(e)
#             print(.jcheck(silent=FALSE))
#         }
#         message("kv: ", kv)
#         encodedKVs[length(encodedKVs)+1] <- kv
#     }
#     message(encodedKVs)
#     assembledQuery <- paste(encodedKVs,collapse="&")
#     message(assembledQuery)
#     
#     return( d1SolrQuery(x, assembledQuery) )
})

setMethod("d1SolrQuery", signature("D1Client", "character"), function(x, solrQuery) {
    
#     packagedQuery <- paste0("?", solrQuery)
#     message("packagedQuery: ", packagedQuery)
#     
#     jInputStream <- x@client$getCN()$query("solr", packagedQuery)
#     
#     data <- J("org/apache/commons/io/IOUtils","toString", jInputStream,"UTF-8")
#     if (!is.null(e<-.jgetEx())) {
#         print("Java exception was raised")
#         print(paste("Exception detail code:", e$getDetail_code()))
#         print(e)
#         print(.jcheck(silent=FALSE))
#     }
#     return(data)
})

#' A method to query the DataONE solr endpoint of the Coordinating Node, and 
#' return a character vector of identifiers.  
#' It expects a fully encoded character string as input (with lucene-reserved 
#' characters backslash escaped and url-reserved characters percent-encoded).
#' @param x  D1Client: representing the DataONE environment being queried
#' @param solrQuery  character: a fully encoded query string 
#' @return a vector of identifiers found
#' @note users should not provide the leading '?' to the query
#' @examples \dontrun{ d1IdentifierSearch(client,"q=%2Bspecies%20population%20diversity" }
#' @export
setGeneric("d1IdentifierSearch", function(x, solrQuery) {
    standardGeneric("d1IdentifierSearch")    
})

setMethod("d1IdentifierSearch", signature("D1Client", "character"), function(x, solrQuery) {
    
#     # empirical testing shows that prepending the 'fl' and 'wt' fields effectively 
#     # negates any other fl or wr values that might be part of the passed in solrQuery
#     # (need to do this for parsing the reponse)
#     finalQuery = paste0("fl=identifier&wt=json&",solrQuery)
#     message("final query: ", finalQuery)
#     jsonResponse <- d1SolrQuery(x, finalQuery)
#     
#     # remove leading and trailing junk that surrounds the identifier list
#     intermediate <- sub("\"}\\]}}","",sub(".*docs\":\\[\\{\"identifier\":\"","",jsonResponse))
#     
#     # if there are no identifiers returned, the intermediate will still have the responseHeader
#     if(grepl("^\\{\"responseHeader\":",intermediate)) {
#         return(character(0))
#     }
#     # split the remaining identifier list by the json cruft
#     result <- unlist(strsplit(intermediate,"\"\\},\\{\"identifier\":\""))
#     return(result)
})


#' Reserve an Identifier in the DataONE System
#' Reserve an identifier for future use in the DataONE System.
#' @param x : D1Client
#' @param id : identifier to reserve
#' @param ... (not yet used)
#' @return true if reserved
#' @export
setGeneric("reserveIdentifier", function(x, id, ...) { 
    standardGeneric("reserveIdentifier")
})

setMethod("reserveIdentifier", signature("D1Client", "character"), function(x, id) {
#     message(paste("Reserving id:", id))
#     
#     # Create a DataONE Identifier
#     pid <- .jnew("org/dataone/service/types/v1/Identifier")
#     pid$setValue(id)
#     
#     # Reserve the specified identifier on the coordinating node.
#     cn <- getCN(x)
#     pid <- cn$reserveIdentifier(pid)
#     if (!is.null(e <- .jgetEx())) {
#         if(e$getDetail_code() == "4210") {
#             print(paste(identifier, "id cannot be used"))
#         } else {
#             print(paste("Exception detail code:", e$getDetail_code()))
#             print(e)
#         }
#         return(FALSE)
#     }
#     
#     message(paste("Reserved pid:", pid$getValue()))
#     return(TRUE)
})



#' Create the Object in the DataONE System
#' 
#' Creates a D1Object on the MemberNode determined by the object's systemMetadata.
#' 
#' @param x : D1Client
#' @param d1Object : the object to create in DataONE
#' @param ... (not yet used)
#' @return TRUE if success
#' 
#' @export
setGeneric("createD1Object", function(x, d1Object, ...) { 
    standardGeneric("createD1Object")
})


setMethod("createD1Object", signature("D1Client", "D1Object"), function(x, d1Object) {
#     message("--> createD1Object(D1Client, D1Object)")
#     
#     # -- Validate everything necessary to create new object.
#     message("    * Validating the D1Object....")
#     if(is.jnull(d1Object)) {
#         print("    ** Cannot create a null object.")
#         return(FALSE)
#     }
#     jD1o <- d1Object@jD1o  
#     
#     sysmeta <- jD1o$getSystemMetadata()
#     if(is.jnull(sysmeta)) {
#         print("    ** Cannot create with a null sysmeta object.")
#         return(FALSE)
#     }
#     
#     if(is.jnull(sysmeta$getIdentifier())) {
#         print("    ** Cannot create with a null identifier.")
#         return(FALSE)
#     }
#     message("    * object valid")
#     
#     ## TODO: uncomment this when /reserve is more reliable
#     ## -- Reserve this identifier
#     #  pid <- sysmeta$getIdentifier()
#     #  id <- pid$getValue()
#     
#     #  if(!reserveIdentifier(x, id)) {
#     #      print(paste("    ** Identifier already exists, or has been reserved: ", id))
#     #      return(FALSE)
#     #  }
#     #  message("    * ID Reserved: ", id)
#     
#     ## -- Connect to the member node and create the object.
#     
#     jNewPid <- x@client$create(x@session, jD1o)
#     if (!is.jnull(e <- .jgetEx())) {
#         print("    ** Java exception was raised")
#         print(paste("Exception detail code:", e$getDetail_code()))
#         print(e)
#         print(.jcheck(silent=FALSE))
#     }
#     message("    * Created.")
#     
#     if(!is.jnull(jNewPid)) {
#         message("      - created pid:", jNewPid$getValue())
#     } else {
#         message("      - pid is null")
#     }
#     message("<--  create(D1Client, D1Object)")
#     return(jNewPid$getValue())
})


#' Create a DataPackage in the DataONE System
#' 
#' @description Creates the D1Objects contained in the DataPackage by calling the createD1Object()
#' on each of the members, as well as assembling the resourceMap object from the
#' recorded relationships, and calling create() on it as well. 
#' Any objects in the data map that have a dataUploaded value are assumed to be 
#' pre-existing in the system, and skipped.
#' 
#' @details The DataPackage describes the collection of data object and their associated 
#' metadata object, with the relationships and members serialized into a document
#' stored under, and retrievable with, the packageId as it's own distinct object.
#' 
#' Members are created serially, and most errors in creating one object will 
#' interrupt the create process for the whole, with the result that some members will 
#' be created, and the remainder not.
#' 
#' @param x D1Client
#' @param dataPackage The DataPackage instance to be submitted to DataONE for creation.
#' @param ... (not yet used)
#' @return NULL
#' @export
setGeneric("createDataPackage", function(x, dataPackage, ...) { 
    standardGeneric("createDataPackage")
})
setMethod("createDataPackage", signature("D1Client", "DataPackage"), function(x, dataPackage ) {
#     message("====> createDataPackage(D1Client,DataPackage")
#     
#     message(paste("    * building the resource map for the", getSize(dataPackage), "members..."))
#     resourceMapString <- dataPackage@jDataPackage$serializePackage()
#     mapObject <- new("D1Object", dataPackage@packageId, resourceMapString, 
#                      "http://www.openarchives.org/ore/terms", x@mn.nodeid)
#     
#     ## TODO: this should not always be the case in the future.  
#     ## access should match the accessPolicy of the metadata objects, yes?
#     setPublicAccess(mapObject)
#     
#     ## add the vector of pids in the dataList to the java DataPackage
#     members <- getIdentifiers(dataPackage)
#     
#     for (pid in members) {
#         message(paste("    * next member to create:", pid))
#         rD1o <- getMember(dataPackage, pid)
#         jUploadDate <- rD1o@jD1o$getSystemMetadata()$getDateUploaded()
#         if (!is.jnull( jUploadDate )) {
#             message("     * SystemMetadata indicates that this object was already created (uploaded ",
#                     jUploadDate$toString(),
#                     "). Skipping create.")
#         }
#         createD1Object(x, rD1o)
#     }
#     
#     message(paste("    * creating the package resource map:", dataPackage@packageId ))
#     createD1Object(x, mapObject)
#     
#     message("<====  createDataPackage(D1Client, DataPackage")
#     
})


#########################################################
### Accessor methods
#########################################################

#' getEndpoint
setGeneric("getEndpoint", function(x, ...) { standardGeneric("getEndpoint")} )

setMethod("getEndpoint", "D1Client", function(x) {
    res <- x@cn$baseUrl
    return(res)
})

#' Get for the member node identifier associated with this client.
#' One Member Node can be associated with the client as the default to which
#' data and metadata are written.
setGeneric("getMNodeId", function(x) { 
    standardGeneric("getMNodeId")
})
setMethod("getMNodeId", signature("D1Client"), function(x) {
    return(x@mn$identifier)
})

#' Set for the member node identifier associated with this client.
#' One Member Node can be associated with the client as the default to which
#' data and metadata are written.
setGeneric("setMNodeId", function(x, id) { 
    standardGeneric("setMNodeId")
})
setMethod("setMNodeId", signature("D1Client", "character"), function(x, id) {
    if(!is.null(id) && id != "") {
        newMN <- getMNode(x@cn, id)
        if (newMN != NULL) {
            x@mn <- newMN   
        } else {
            message(paste0("Member Node not found: ", id))
        }
    }
})


#' Get a member node client based on its node identifier
#' 
setGeneric("getMN", function(x, nodeid, ...) { 
    standardGeneric("getMN")
})
setMethod("getMN", signature("D1Client"), function(x, ...) {
    return(x@mn)
})
setMethod("getMN", signature("D1Client", "character"), function(x, nodeid) {
    if(is.null(nodeid) || (nodeid == "")) {
        newMN <- getMNode(x@cn, nodeid)
        if (newMN == NULL) {
            message(paste0("Member Node not found: ", id))
        }
    }
    return(newMN)
})


#' Get a coordinating node client.
setGeneric("getCN", function(x) { 
    standardGeneric("getCN")
})
setMethod("getCN", signature("D1Client"), function(x) {
    return(x@cn)
})


setGeneric("listMemberNodes", function(x) {
    standardGeneric("listMemberNodes")
})

setMethod("listMemberNodes", signature("D1Client"), function(x) {
    return (listNodes(x))
})

#########################################################
### Utility methods
#########################################################

#' Convert a DataFrame to Standard CSV
#' @param df the dataFrame
#' @param ... additional params passed to write.csv
#' @return the dataframe serialized as a .csv
#' @export
setGeneric("convert.csv", function(x, ...) {
    standardGeneric("convert.csv")
})

setMethod("convert.csv", signature(x="D1Client"), function(x, df, ...) {
    con <- textConnection("data", "w")
    write.csv(df, file=con, row.names = FALSE, col.names = TRUE, ...)
    close(con)
    csvdata <- paste(data, collapse="\n")
    return(csvdata)
})

#' Encode the Input for Solr Queries
#' 
#' Treating all special characters and spaces as literals, backslash escape special
#' characters, and double-quote if necessary 
#' @param segment : a string to encode
#' @return the encoded form of the input
#' @examples encodeSolr("this & that")
#' @export
setGeneric("encodeSolr", function(x, segment, ... ) {
    standardGeneric("encodeSolr")
})

setMethod("encodeSolr", signature(x="D1Client", segment="character"), function(x, segment, ...) {
    inter <- gsub("([-+:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","\\\\\\1",segment, perl=TRUE) 
    if (grepl(" ",inter)) {
        return(paste0("\"",inter,"\""))
    }
    return(inter)
})




#' Encode the Input for a URL Query Segment
#' 
#' Encodes the characters of the input so they are not interpretted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param querySegment : a string to encode
#' @return the encoded form of the input
#' @examples fullyEncodedQuery <- paste0("q=id:",encodeUrlQuery(client,encodeSolr("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5")))
#' @export
setGeneric("encodeUrlQuery", function(x, querySegment, ...) {
    standardGeneric("encodeUrlQuery")
})

setMethod("encodeUrlQuery", signature(x="D1Client", querySegment="character"), function(x, querySegment, ...) {
    
    #    luceneExample <- "+pool +ABQ\\:Bernalillo \\[NM\\] -sharks \"kids & adults = fun day\"" 
    #    luceneReservedCharExample <- "example__\\+_\\-_\\&_\\|_\\!_\\^_\\~_\\*_\\?_\\:_\\\"_\\(_\\)_\\{_\\}_\\[_\\]____"
    
#     encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlQuerySegment", querySegment)
#     if (!is.null(e<-.jgetEx())) {
#         print("Java exception was raised")
#         print(.jcheck(silent=FALSE))
#     }
#     return(encoded)
    
    ## an R-only alternate implementation that only would work for ASCII characters
    ## (may need to check the behavior of {,},[,] - they may need to be hidden also)
    #    escaped <- gsub("\\\\([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","%5C\\1",querySegment, perl=TRUE)
    #    escaped <- gsub("%5C&","%5C%26",solrQuery)  ##  need to hide the ampersand from the web server
    #    escaped   <- gsub("%5C\\+","%5C%2B",solrQuery)  ## need to hide the + from the web server
})




#' Encode the Input for a URL Path Segment
#' 
#' Encodes the characters of the input so they are not interpretted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param pathSegment : a string to encode
#' @return the encoded form of the input
#' @examples fullyEncodedPath <- paste0("cn/v1/object/",encodeUrlPath("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5"))
#' @export
setGeneric("encodeUrlPath", function(x, pathSegment, ...) {
    standardGeneric("encodeUrlPath")
})

setMethod("encodeUrlPath", signature(x="D1Client", pathSegment="character"), function(x, pathSegment, ...) {
#     encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlPathSegment", pathSegment)
#     if (!is.null(e<-.jgetEx())) {
#         print("Java exception was raised")
#         print(.jcheck(silent=FALSE))
#     }
#     return(encoded)
})

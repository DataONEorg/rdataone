#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2012
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

### This file contains the methods and accessors for D1Client objects

#########################################################
### MNAuthentication methods
#########################################################


#########################################################
### MNRead methods
#########################################################

## getPackage
setGeneric("getPackage", function(x, identifier, ...) { 
  standardGeneric("getPackage")
})

setMethod("getPackage", signature("D1Client", "character"), function(x, identifier) {

  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  message(paste("@@ D1Client-methods.R 50: getPackage for", identifier))
  ## jNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
  ## jNodeRef$setValue(x@mn.nodeid)
  ## message(paste("@@ D1Client-methods.R 51: nodeReference from rD1Client", x@mn.nodeid))
  message(paste("@@ D1Client-methods.R 51: calling java DataPackage download method..." ))
  jDataPackage <- J("org/dataone/client/DataPackage")$download(jPid)
  if (!is.null(e<-.jgetEx())) {
	  print("Java exception was raised")
	  print(.jcheck(silent=FALSE))
  }
  dp <- new(Class="DataPackage",jDataPackage=jDataPackage)
  return(dp)
})

solrEscapeCharacters <- function(querySegment) {
    backslashed <- gsub("\\\\([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","%5C\\1",querySegment, perl=TRUE)
    return(backslashed)
}

#' encodes the reserved characters in a url query segment
urlEncodeQuerySegment <- function(querySegment) {

#    luceneExample <- "+pool +ABQ\\:Bernalillo \\[NM\\] -sharks \"kids & adults = fun day\"" 
#    luceneReservedCharExample <- "example__\\+_\\-_\\&_\\|_\\!_\\^_\\~_\\*_\\?_\\:_\\\"_\\(_\\)_\\{_\\}_\\[_\\]____"

    message("querySegment: ", querySegment)
    encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlQuerySegment", querySegment)
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    message("encoded: ", encoded)
    return(encoded)
    
    ## an R-only alternate implementation that only would work for ASCII characters
    ## (may need to check the behavior of {,},[,] - they may need to be hidden also)
    #    escaped <- gsub("\\\\([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","%5C\\1",querySegment, perl=TRUE)
    #    escaped <- gsub("%5C&","%5C%26",solrQuery)  ##  need to hide the ampersand from the web server
    #    escaped   <- gsub("%5C\\+","%5C%2B",solrQuery)  ## need to hide the + from the web server
}



setGeneric("d1SolrQuery", function(x, solrQuery) { 
            standardGeneric("d1SolrQuery")
        })

#' A method to query the DataONE solr endpoint of the Coordinating Node.
#' It expects any lucene reserved characters to already be escaped with backslash.
#' @param x  the D1Client (environment) being queried
#' @param solrQuery  list
#' @returnType character
#' @return the solr response (XML)
#' @examples \dontrun{ d1SolrQuery(client,list(q="+species population diversity", fl="identifier")) }
#' @author rnahf
#' @export
setMethod("d1SolrQuery", signature("D1Client", "list"), function(x, solrQuery) {

    encodedKVs <- character()
    for(key in attributes(solrQuery)$names) {
        kv <- paste0(urlEncodeQuerySegment(key), "=", urlEncodeQuerySegment(solrQuery[[key]]))
        if (!is.null(e<-.jgetEx())) {
            print("Java exception was raised")
            print(paste("Exception detail code:", e$getDetail_code()))
            print(e)
            print(.jcheck(silent=FALSE))
        }
        message("kv: ", kv)
        encodedKVs[length(encodedKVs)+1] <- kv
    }
    message(encodedKVs)
    assembledQuery <- paste(encodedKVs,collapse="&")
    message(assembledQuery)
    
    return( d1SolrQuery(x, assembledQuery) )
})


#' A method to query the DataONE solr endpoint of the Coordinating Node.
#' It expects a fully encoded character string as input (with lucene-reserved 
#' characters backslash escaped and url-reserved characters %-encoded).
#' @param x  D1Client: representing the DataONE environment being queried
#' @param solrQuery  character: a fully encoded query string 
#' @returnType character
#' @return the solr response (XML)
#' @note users should not provide the leading '?' to the query
#' @examples \dontrun{ d1SolrQuery(client,"q=%2Bspecies%20population%20diversity%26fl=identifier" }
#' @author rnahf
#' @export
setMethod("d1SolrQuery", signature("D1Client", "character"), function(x, solrQuery) {
    
    packagedQuery <- paste0("?", solrQuery)
    message("packagedQuery: ", packagedQuery)
    
    jInputStream <- x@client$getCN()$query("solr", packagedQuery)
    
    data <- J("org/apache/commons/io/IOUtils","toString", jInputStream,"UTF-8")
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(paste("Exception detail code:", e$getDetail_code()))
        print(e)
        print(.jcheck(silent=FALSE))
    }
    return(data)
})


setGeneric("d1IdentifierSearch", function(x, solrQuery) {
	standardGeneric("d1IdentifierSearch")    
})


#' A method to query the DataONE solr endpoint of the Coordinating Node, and 
#' return a character vector of identifiers.  
#' It expects a fully encoded character string as input (with lucene-reserved 
#' characters backslash escaped and url-reserved characters percent-encoded).
#' @param x  D1Client: representing the DataONE environment being queried
#' @param solrQuery  character: a fully encoded query string 
#' @returnType character
#' @return a vector of identifiers found
#' @note users should not provide the leading '?' to the query
#' @examples \dontrun{ d1IdentifierSearch(client,"q=%2Bspecies%20population%20diversity" }
#' @author rnahf
#' @export
setMethod("d1IdentifierSearch", signature("D1Client", "character"), function(x, solrQuery) {
	
	## empirical testing shows that prepending the 'fl' and 'wt' fields effectively 
	## negates any other fl or wr values that might be part of the passed in solrQuery
	## (need to do this for parsing the reponse)
	finalQuery = paste0("fl=identifier&wt=json&",solrQuery)
	message("final query: ", finalQuery)
	jsonResponse <- d1SolrQuery(x, finalQuery)
	
	## remove leading and trailing junk that surrounds the identifier list
	intermediate <- sub("\"}\\]}}","",sub(".*docs\":\\[\\{\"identifier\":\"","",jsonResponse))
	
	## if there are no identifiers returned, the intermediate will still have the responseHeader
	if(grepl("^\\{\"responseHeader\":",intermediate)) {
		return(character(0))
	}
	## split the remaining identifier list by the json cruft
	result <- unlist(strsplit(intermediate,"\"\\},\\{\"identifier\":\""))
	return(result)
})


## getD1Object
setGeneric("getD1Object", function(x, identifier, ...) { 
  standardGeneric("getD1Object")
})

setMethod("getD1Object", "D1Client", function(x, identifier) {
  client <- x@client
  session <- x@session
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  ## jNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
  ## jNodeRef$setValue(x@mn.nodeid)

  # Use libclient D1Object to get the object
  jD1Object <- J("org/dataone/client/D1Object")$download(jPid)
  .jcheck(silent = FALSE)
  
  d1o <- new("D1Object",jD1Object)
  return(d1o)
})


## reserveIdentifier
setGeneric("reserveIdentifier", function(x, id, ...) { 
  standardGeneric("reserveIdentifier")
})

setMethod("reserveIdentifier", signature("D1Client", "character"), function(x, id) {
  message(paste("Reserving id:", id))

  # Create a DataONE Identifier
  pid <- .jnew("org/dataone/service/types/v1/Identifier")
  pid$setValue(id)

  # Reserve the specified identifier on the coordinating node.
  cn <- getCN(x)
  pid <- cn$reserveIdentifier(pid)
  if (!is.null(e <- .jgetEx())) {
    if(e$getDetail_code() == "4210") {
      print(paste(identifier, "id cannot be used"))
    } else {
      print(paste("Exception detail code:", e$getDetail_code()))
      print(e)
    }
    return(FALSE)
  }

  message(paste("Reserved pid:", pid$getValue()))
  return(TRUE)
})



setGeneric("createD1Object", function(x, d1Object, ...) { 
  standardGeneric("createD1Object")
})

#' creates a D1Object on the MemberNode determined by the object's systemMetadata.
#' returns FALSE on exceptions
#' 
setMethod("createD1Object", signature("D1Client", "D1Object"), function(x, d1Object) {
    message("--> createD1Object(D1Client, D1Object)")

    ## -- Validate everything necessary to create new object.
    message("    * Validating the D1Object....")
    if(is.jnull(d1Object)) {
        print("    ** Cannot create a null object.")
        return(FALSE)
    }
    jD1o <- d1Object@jD1o  
  
    sysmeta <- jD1o$getSystemMetadata()
    if(is.jnull(sysmeta)) {
        print("    ** Cannot create with a null sysmeta object.")
        return(FALSE)
    }
    
    if(is.jnull(sysmeta$getIdentifier())) {
        print("    ** Cannot create with a null identifier.")
        return(FALSE)
    }
    message("    * object valid")

    ## TODO: uncomment this when /reserve is more reliable
    ## -- Reserve this identifier
    #  pid <- sysmeta$getIdentifier()
    #  id <- pid$getValue()
  
    #  if(!reserveIdentifier(x, id)) {
    #      print(paste("    ** Identifier already exists, or has been reserved: ", id))
    #      return(FALSE)
    #  }
    #  message("    * ID Reserved: ", id)

    ## -- Connect to the member node and create the object.

    jNewPid <- x@client$create(x@session, jD1o)
    if (!is.jnull(e <- .jgetEx())) {
        print("    ** Java exception was raised")
        print(paste("Exception detail code:", e$getDetail_code()))
        print(e)
        print(.jcheck(silent=FALSE))
    }
    message("    * Created.")

    if(!is.jnull(jNewPid)) {
        message("      - created pid:", jNewPid$getValue())
    } else {
        message("      - pid is null")
    }
    message("<--  create(D1Client, D1Object)")
    return(is.jnull(jNewPid))
})


setGeneric("createDataPackage", function(x, dataPackage, ...) { 
            standardGeneric("createDataPackage")
        })
setMethod("createDataPackage", signature("D1Client", "DataPackage"), function(x, dataPackage ) {
  message("====> createDataPackage(D1Client,DataPackage")
            
  message(paste("    * building the resource map for the", getSize(dataPackage), "members..."))
  resourceMapString <- dataPackage@jDataPackage$serializePackage()
  mapObject <- new("D1Object", dataPackage@packageId, resourceMapString, 
		  "http://www.openarchives.org/ore/terms", x@mn.nodeid)
  
  ## TODO: this should not always be the case in the future.  
  ## access should match the accessPolicy of the metadata objects, yes?
  setPublicAccess(mapObject)
  
  ## add the vector of pids in the dataList to the java DataPackage
  members <- getIdentifiers(dataPackage)
  
  for (pid in members) {
      message(paste("    * next member to create:", pid))
      rD1o <- getMember(dataPackage, pid)
      jUploadDate <- rD1o@jD1o$getSystemMetadata()$getDateUploaded()
      if (!is.jnull( jUploadDate )) {
          message("     * SystemMetadata indicates that this object was already created (uploaded ",
                    jUploadDate$toString(),
                    "). Skipping create.")
      }
      createD1Object(x, rD1o)
  }
  
  message(paste("    * creating the package resource map:", dataPackage@packageId ))
  createD1Object(x, mapObject)
  
  message("<====  createDataPackage(D1Client, DataPackage")

})

 
#########################################################
### Accessor methods
#########################################################

## getEndpoint
setGeneric("getEndpoint", function(x, ...) { standardGeneric("getEndpoint")} )

setMethod("getEndpoint", "D1Client", function(x) {
  res <- x@endpoint
  return(res)
})

## Getter and Setter for the node id.
##
setGeneric("getMNodeId", function(x) { 
  standardGeneric("getMNodeId")
})
setMethod("getMNodeId", signature("D1Client"), function(x) {
  return(x@mn.nodeid)
})

setGeneric("setMNodeId", function(x, id) { 
  standardGeneric("setMNodeId")
})
setMethod("setMNodeId", signature("D1Client", "character"), function(x, id) {
  if(!is.null(id) && id != "") {
    x@mn.nodeid <- id
  }
})


## Get a member node client.
## Allow for optionally passing the nodeid.
setGeneric("getMN", function(x, nodeid, ...) { 
    standardGeneric("getMN")
})
setMethod("getMN", signature("D1Client"), function(x, ...) {
  mn <- getMN(x, x@mn.nodeid)
})
setMethod("getMN", signature("D1Client", "character"), function(x, nodeid) {
  # Validate nodeid.
  if(is.null(nodeid) || (nodeid == "")) {
    print("ERROR: No member node id is defined.")
    return(.jnull("org/dataone/client/MNode"))
  }

  # Build a NodeReference out of the id and return a MN client.
  node.ref <- .jnew("org/dataone/service/types/v1/NodeReference")
  node.ref$setValue(nodeid)
  mn <- J("org/dataone/client/D1Client")$getMN(node.ref)

  return(mn)
})


## Get a coordinating node client.
setGeneric("getCN", function(x) { 
  standardGeneric("getCN")
})
setMethod("getCN", signature("D1Client"), function(x) {
  cn <- J("org/dataone/client/D1Client")$getCN()
  return(cn)
})




setGeneric("listMemberNodes", function(x) {
    standardGeneric("listMemberNodes")
})

setMethod("listMemberNodes", signature("D1Client"), function(x) {
    mnIDset <- x@client$getCN()$listNodeIds()$toArray(.jarray(""))
    ## remove CN
    return (mnIDset)
})


#########################################################
### Utility methods
#########################################################

## convert.csv
# setGeneric("convert.csv", function(x, ...) {
#   standardGeneric("convert.csv")} 
# )
# 
# setMethod("convert.csv", signature(x="D1Client"), function(x, df, ...) {
#   con <- textConnection("data", "w")
#   write.csv(df, file=con, row.names = FALSE, col.names = TRUE, ...)
#   close(con)
#   csvdata <- paste(data, collapse="\n")
#   return(csvdata)
# })

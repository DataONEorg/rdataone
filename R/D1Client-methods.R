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

setMethod("getPackage", "D1Client", function(x, identifier) {
    client <- x@client
    session <- x@session
    pid <- .jnew("org/dataone/service/types/v1/Identifier")
    pid$setValue(identifier)

    cnode <- client$getCN()

    # Make sure this is the correct type.
    jSysMeta <- cnode$getSystemMetadata(pid)
    if (!is.null(e<-.jgetEx())) {
	print("Java exception was raised")
	print(.jcheck(silent=TRUE))
    } else {
        jObjectFormatId <- jSysMeta$getFormatId()
	formatId <- jObjectFormatId$getValue()
	if(formatId != "http://www.openarchives.org/ore/terms") {
	    print(paste("ERROR: Object is not correct format: ", formatId))
	    return(NULL)
	}
    }

    # Resolve the ID to find the MN to use
    jD1Object <- J("org/dataone/client/D1Object")$download(pid)
    if (!is.null(e<-.jgetEx())) {
	print("Java exception was raised")
	print(.jcheck(silent=FALSE))
    } else if(is.jnull(jD1Object)) {
        print(paste("Couldn't download:", pid))
	return(NULL)
    }

    # Convert to data package.
    databytes <- jD1Object$getData()
    jString <- .jnew("java/lang/String", databytes)
    if(FALSE) {
	.jcheck(silent = FALSE)
	print(paste("Object is",  jString$length(), "characters in size"))
	print(jString)
    }
    jDataPackage <- J("org/dataone/client/DataPackage")$deserializePackage(jString)
    dp <- createDataPackage(jDataPackage, jSysMeta)
    return(dp)
})


## getD1Object
setGeneric("getD1Object", function(x, identifier, ...) { 
    standardGeneric("getD1Object")
})

setMethod("getD1Object", "D1Client", function(x, identifier) {
    client <- x@client
    session <- x@session
    pid <- .jnew("org/dataone/service/types/v1/Identifier")
    pid$setValue(identifier)

    # Use libclient D1Object to get the object
    d1Object <- J("org/dataone/client/D1Object")$download(pid)
    .jcheck(silent = FALSE)

    return(d1Object)
})


## create
setGeneric("create", function(x, J_d1Object, ...) { 
    standardGeneric("create")
})

setMethod("create", "D1Client", function(x, J_d1Object) {
    # -- Validate everything necessary to create new object..
    if(is.jnull(J_d1Object)) {
        print("Cannot create a null object.")
	return(FALSE)
    }
    if(is.jnull(J_d1Object$getSystemMetadata())) {
        print("Cannot create with a null sysmeta object.")
	return(FALSE)
    }

    #print("getSystemMetaData")
    sysmeta <- J_d1Object$getSystemMetadata()
    if (!is.null(e<-.jgetEx())) {
	print("Java exception was raised")
	print(.jcheck(silent=TRUE))
    }

    if(is.jnull(sysmeta$getIdentifier())) {
        print("Cannot create with a null identifier.")
	return(FALSE)
    }
    pid <- sysmeta$getIdentifier()

    cn <- getCN(x)
    oll <- cn$resolve(x@session, pid)
    if(!is.jnull(d1Client@mnRef)) {
        print(paste(pid, "- object already exists."))
	return(FALSE)
    }

    if(is.jnull(d1Client@mnRef)) {
        print("Cannot create an object without a Member Node defined.")
	return(FALSE)
    }

    object <- .jnew("java/io/ByteArrayInputStream", J_d1Object$getData())
    mn <- getMN(x)
    newPid <- mn$create(x@session, pid, object, sysmeta)

    return(is.jnull(newPid))
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


#########################################################
### Utility methods
#########################################################

## convert.csv
setGeneric("convert.csv", function(x, ...) {
    standardGeneric("convert.csv")} 
)

setMethod("convert.csv", signature(x="D1Client"), function(x, df) {
    con <- textConnection("data", "w")
    write.csv(df, file=con, row.names = FALSE)
    close(con)
    csvdata <- paste(data, collapse="\n")
    return(csvdata)
})

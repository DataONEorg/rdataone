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

    # Resolve the ID to find the MN to use
    cnode <- client$getCN()
    print("Trying resolve operation....")
    oll <- cnode$resolve(session, pid)
    olist <- oll$getObjectLocationList()
    objloc <- olist$get(as.integer(0))
    node <- objloc$getNodeIdentifier()
    nodeid <- node$getValue()
    #print(nodeid)
    nodeurl <- objloc$getBaseURL()
    print(nodeurl)

    # Get the sysmeta for this GUID to decide how to handle it
    print("Trying sysmeta operation....")
    sysmeta <- cnode$getSystemMetadata(session, pid)
    .jcheck(silent = FALSE)
    fmtid <- sysmeta$getFormatId()
    oformat = cnode$getFormat(fmtid)
    #print(oformat$toString())

    # Now get the object from the correct MN
    print("Trying read operation....")
    mnode <- client$getMN(nodeurl)
    datastream <- mnode$get(session, pid) 
    .jcheck(silent = FALSE)
    ioUtils <-  .jnew("org/apache/commons/io/IOUtils") 
    .jcheck(silent = FALSE)
    rdata <- ioUtils$toString(datastream)

    # Load the data into a dataframe
    df <- read.table(textConnection(rdata), header = TRUE, sep = ",", na.strings = "-999")
    #df <- read.table(textConnection(rdata), header = FALSE, skip=27)

    scimeta <- "Placeholder string for science metadata, waiting to implement lookup of sci metadata from describedBy field in sysmeta"
    dp <- DataPackage(identifier, sysmeta, scimeta)
    dp <- addData(dp, df)
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

    # Use libclient D1Object to get bytes of object
    # First, find a member node
    cn <- client$getCN()
    oll <- cn$resolve(session, pid)
    if (!is.null(e<-.jgetEx())) {
	print("Java exception was raised")
	print(.jcheck(silent=TRUE))
    }
    olist <- oll$getObjectLocationList()
    objloc <- olist$get(as.integer(0))
    node <- objloc$getNodeIdentifier()
    nodeid <- node$getValue()

    # Second, get the object.
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
print("getSystemMetaData")
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

setGeneric("getMNodeId", function(x, ...) { 
    standardGeneric("getMNodeId")
})
setMethod("getMNodeId", "D1Client", function(x) {
    if(is.jnull(x@mnRef)) {
        return("")
    } else {
        return(x@mnRef$getValue())
    }
})

setGeneric("setMNodeId", function(x, ...) { 
    standardGeneric("setMNodeId")
})
setMethod("setMNodeId", "D1Client", function(x, id) {
    if(id != "") {
        nodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
	nodeRef$setValue(id)
	x@mnRef <- nodeRef
    }
})

setGeneric("getMN", function(x, ...) { 
    standardGeneric("getMN")
})
setMethod("getMN", "D1Client", function(x) {
    if(is.jnull(x@mnRef)) {
        e <- .jnew("org/dataone/service/exceptions/ServiceFailure", "1102", "Invalid Request: No member node defined.")
	.jthrow(e)
    }

    mn <- J("org/dataone/client/D1Client")$getMN(x@mnRef)
    return(mn)
})

setGeneric("getCN", function(x, ...) { 
    standardGeneric("getCN")
})
setMethod("getCN", "D1Client", function(x) {
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

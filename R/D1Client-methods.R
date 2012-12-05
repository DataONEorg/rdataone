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
  client <- x@client
  session <- x@session
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  print(paste("@@ D1Client-methods.R 50: getPackage for", identifier))
  ## jNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
  ## jNodeRef$setValue(x@mn.nodeid)
  ## print(paste("@@ D1Client-methods.R 51: nodeReference from rD1Client", x@mn.nodeid))
  print(paste("@@ D1Client-methods.R 51: calling java DataPackage download method..." ))
  jDataPackage <- J("org/dataone/client/DataPackage")$download(jPid)
  if (!is.null(e<-.jgetEx())) {
	  print("Java exception was raised")
	  print(.jcheck(silent=FALSE))
  }
  dp <- new(Class="DataPackage",jDataPackage=jDataPackage)
  return(dp)
})


## createPackage
## setGeneric("createPackage", function(x, data_package, ...) { 
##   standardGeneric("createPackage")
## })
## 
## setMethod("createPackage", signature("D1Client", "DataPackage"),
##     function(x, data_package) {
##   client <- x@client
##   session <- x@session
##   pid <- .jnew("org/dataone/service/types/v1/Identifier")
##   pid$setValue(identifier)
## })


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
  print(paste("Reserving id:", id))

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

  print(paste("Reserved pid:", pid$getValue()))
  return(TRUE)
})


## create
setGeneric("create", function(x, object, ...) { 
  standardGeneric("create")
})

setMethod("create", signature("D1Client", "D1Object"), function(x, object) {
  VERBOSE <- TRUE
  if (VERBOSE) print("--> create(D1Client, D1Object)")

  ## -- Validate everything necessary to create new object.
  if(is.jnull(object)) {
    print("    ** Cannot create a null object.")
    return(FALSE)
  }
  jD1o <- object@jD1o  
  
  if (VERBOSE) print("    * The object is not null.")
  sysmeta <- jD1o$getSystemMetadata()
  if(is.jnull(sysmeta)) {
    print("    ** Cannot create with a null sysmeta object.")
    return(FALSE)
  }
  if (VERBOSE) print("    * The sysmeta is not null.")
  if(is.jnull(sysmeta$getIdentifier())) {
    print("    ** Cannot create with a null identifier.")
    return(FALSE)
  }

  ## -- Reserve this identifier
  pid <- sysmeta$getIdentifier()
  id <- pid$getValue()
  if(!reserveIdentifier(x, id)) {
    print(paste("    ** Identifier already exists, or has been reserved: ", id))
    return(FALSE)
  }
  if (VERBOSE) print(paste("    * Reserved.", id))

  
  
  ## -- Connect to the member node and create the object.
  jNewPid <- x@client$create(x@session, jD1o)
            
  ## the old way, of creating, but it doesn't respect the object's MN reference            
  ## mn <- getMN(x)
  ## if(is.jnull(mn)) {
  ##   return(FALSE)
  ## }
  ## print(mn)
  ## jDataIS <- .jnew("java/io/ByteArrayInputStream", getData(object))
  ## print("@@ D1Client-methods 40:")
  ## 
  ## jNewPid <- mn$create(x@session, pid, jDataIS, sysmeta)
  
  print("@@ D1Client-methods 41:")
  if (!is.jnull(e <- .jgetEx())) {
    print("    ** Java exception was raised")
    print(.jcheck(silent=FALSE))
  }
  if (VERBOSE) print("    * Created.")

  if(!is.jnull(jNewPid)) {
    if (VERBOSE) print(paste("      - created pid:", jNewPid$getValue()))
  } else {
    if (VERBOSE) print("      - pid is null")
  }

  if (VERBOSE) print("<--  create(D1Client, D1Object)")
  return(is.jnull(jNewPid))
})

## setGeneric("create", function(x, rPackage, ...) { 
##   standardGeneric("create")
## })


setMethod("create", signature("D1Client", "DataPackage"), function(x, object ) {
  print("====> create(D1Client,DataPackage")
            
  print(paste("    * building the resource map for the", getSize(object), "members..."))
  resourceMapString <- object@jDataPackage$serializePackage()
  mapObject <- new("D1Object", object@packageId, resourceMapString, 
		  "http://www.openarchives.org/ore/terms", x@mn.nodeid)
  
  ## TODO: this should not always be the case in the future.  
  ## access should match the accessPolicy of the metadata objects, yes?
  setPublicAccess(mapObject)
  
  ## add the vector of pids in the dataList to the java DataPackage
  members <- getIdentifiers(object)
  
  for (pid in members) {
      print(paste("    * next member to create:", pid))
      rD1o <- getMember(object, pid)
      create(x, rD1o)
  }
  print(paste("    * creating the package resource map:", object@packageId ))
  create(x, mapObject)
  
  print("<====  create(D1Client, DataPackage")

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

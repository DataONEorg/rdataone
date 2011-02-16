### This file contains the methods and accessors for D1Client objects

#########################################################
### Authentication methods
#########################################################

## login
setGeneric("login", function(x, username, password, ...) { 
    standardGeneric("login")
})

setMethod("login", "D1Client", function(x, username, password, mnUrl) {
    nodeurl <- getEndpoint(x)
    x@username <- username 
    cli <-  x@cli
    print("Attempting login...")
    mn <- cli$getMN(mnUrl)
    token <- mn$login(username, password) 
    .jcheck(silent = FALSE)
    print("login succeeded.")
    x@token <- token
    print(c("Token from x is: ", x@token$getToken()))
    return(x)
})

#########################################################
### CRUD methods
#########################################################

## getPackage
setGeneric("getPackage", function(x, identifier, ...) { 
    standardGeneric("getPackage")
})

setMethod("getPackage", "D1Client", function(x, identifier) {
   cli <- x@cli
   token <- x@token
   guid <- .jnew("org/dataone/service/types/Identifier")
   guid$setValue(identifier)

   # Resolve the ID to find the MN to use
   cnode <- cli$getCN()
   print("Trying resolve operation....")
   oll <- cnode$resolve(token, guid)
   olist <- oll$getObjectLocationList()
   objloc <- olist$get(as.integer(0))
   node <- objloc$getNodeIdentifier()
   #nodeid <- node$getValue()
   #print(nodeid)
   nodeurl <- objloc$getBaseURL()
   print(nodeurl)

   # Get the sysmeta for this GUID to decide how to handle it
   print("Trying sysmeta operation....")
   sysmeta <- cnode$getSystemMetadata(token, guid)
   .jcheck(silent = FALSE)
   oformat = sysmeta$getObjectFormat()
   #print(oformat$toString())

   # Now get the object from the correct MN
   print("Trying read operation....")
   mnode <- cli$getMN(nodeurl)
   datastream <- mnode$get(token, guid) 
   .jcheck(silent = FALSE)
   iou <-  .jnew("org/apache/commons/io/IOUtils") 
   .jcheck(silent = FALSE)
   rdata <- iou$toString(datastream)

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
   cli <- x@cli
   token <- x@token
   guid <- .jnew("org/dataone/service/types/Identifier")
   guid$setValue(identifier)

   #nodeurl <- "http://knb-mn.ecoinformatics.org/knb/d1"
   nodeurl <- "http://knb-test-1.dataone.org/knb/d1"

   # Now get the object from the correct MN
   print("Trying read operation....")
   mnode <- cli$getMN(nodeurl)
   datastream <- mnode$get(token, guid) 
   .jcheck(silent = FALSE)
   iou <-  .jnew("org/apache/commons/io/IOUtils") 
   .jcheck(silent = FALSE)
   rdata <- iou$toString(datastream)

   # Load the data into a dataframe
   #df <- read.table(textConnection(rdata), header = TRUE, sep = ",", na.strings = "-999")
   #df <- read.table(textConnection(rdata), header = FALSE, skip=27)

   sysmeta <- mnode$getSystemMetadata(token, guid)
   scimeta <- "Placeholder string for science metadata, waiting to implement lookup of sci metadata from describedBy field in sysmeta"
   dp <- DataPackage(identifier, sysmeta, scimeta)
   dp <- addData(dp, rdata)
   return(dp)
})

## createD1Object
setGeneric("createD1Object", function(x, identifier, ...) { 
    standardGeneric("createD1Object")
})

setMethod("createD1Object", "D1Client", function(x, identifier, data) {
   cli <- x@cli
   token <- x@token
   nodeurl <- "http://knb-test-1.dataone.org/knb/d1"

   # Create system metadata
   #Identifier id, byte[] data, ObjectFormat format, String submitter, String
   #nodeId, String[] describes, String[] describedBy
   guid <- .jnew("org/dataone/service/types/Identifier")
   guid$setValue(identifier)

   #dataArray <- .jarray(unlist(sapply(data, charToRaw), use.names=FALSE))
   #print(str(dataArray))
   iou <-  .jnew("org/apache/commons/io/IOUtils") 
   barr <- iou$toByteArray(toString(data))
   print(str(barr))

   format <- "text/csv"
   #submitter <- x@username
   submitter <- "uid=kepler,o=unaffiliated,dc=ecoinformatics,dc=org"
   nodeId <- "http://knb-test-1.dataone.org"
   describes <- .jarray(c("foo.1.1"))
   describedBy <- .jarray(c("foo.2.1"))

   # Now create the object with the sysmeta values
   #print(.jconstructors("org/dataone/client/D1Object"))
   d1object <- .jnew("org/dataone/client/D1Object", guid, barr, format, submitter, nodeId, describes, describedBy, check=FALSE)
   if (!is.null(e<-.jgetEx())) {
       print("Java exception was raised")
       print(.jcheck(silent=TRUE))
       print(.jcheck(silent=TRUE))
       print(e)
   }
   print(show(d1object))
   newId <- d1object$getIdentifier()
   print("ID of d1object:")
   print(newId$getValue())

   #dp <- DataPackage(identifier, sysmeta, scimeta)
   #dp <- addData(dp, rdata)
   #return(dp)
   return(d1object)
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

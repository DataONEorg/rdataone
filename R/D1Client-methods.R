### This file contains the methods and accessors for D1Client objects

#########################################################
### Authentication methods
#########################################################

## login
setGeneric("login", function(x, username, password, ...) { 
    standardGeneric("login")
})

setMethod("login", "D1Client", function(x, username, password) {
    nodeurl <- getEndpoint(x)
    x@username <- username 
    cli <-  x@cli
    print("Attempting login...")
    token <- cli$login(username, password) 
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
   oll <- cnode$resolve(token, guid)
   olist <- oll$getObjectLocationList()
   objloc <- olist$get(as.integer(0))
   node <- objloc$getNodeIdentifier()
   #nodeid <- node$getValue()
   #print(nodeid)
   nodeurl <- objloc$getBaseURL()
   print(nodeurl)

   # Get the sysmeta for this GUID to decide how to handle it
   sysmeta <- cnode$getSystemMetadata(token, guid)
   .jcheck(silent = FALSE)
   oformat = sysmeta$getObjectFormat()
   #print(oformat$toString())

   # Now get the object from the correct MN
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


#########################################################
### Accessor methods
#########################################################

## getEndpoint
setGeneric("getEndpoint", function(x, ...) { standardGeneric("getEndpoint")} )

setMethod("getEndpoint", "D1Client", function(x) {
    res <- x@endpoint
    return(res)
})

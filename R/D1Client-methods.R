### This file contains the methods and accessors for D1Client objects

#########################################################
### MNAuthentication methods
#########################################################

## login
setGeneric("login", function(x, username, password, ...) { 
    standardGeneric("login")
})

#setMethod("login", "D1Client", function(x, username, password, mnUrl) {
#    nodeurl <- getEndpoint(x)
#    x@username <- username 
#    client <-  x@client
#    print("Attempting login...")
#    mn <- client$getMN(mnUrl)
#    session <- mn$login(username, password) 
#    .jcheck(silent = FALSE)
#    print("login succeeded.")
#    x@session <- session
#    print(c("session from x is: ", x@session$getToken()))
#    return(x)
#})

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
   #nodeid <- node$getValue()
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
   #print("Trying to get the object ...")
   d1obj <- J("org/dataone/client/D1Object")$download(pid)
   databytes <- d1obj$getData() 
   .jcheck(silent = FALSE)

   #print("Convert to string...")
   jString <-  .jnew("java/lang/String", databytes) 
   .jcheck(silent = FALSE)
   rdata <- jString$toString()

   #print("Pull out sysmeta...")
   sysmeta <- d1obj$getSystemMetadata()
   scimeta <- "Placeholder string for science metadata, waiting to implement lookup of sci metadata from describedBy field in sysmeta"
   dp <- DataPackage(identifier, sysmeta, scimeta)
   dp <- addData(dp, rdata)
   return(dp)
})

## createD1Object
setGeneric("createD1Object", function(x, identifier, ...) { 
    standardGeneric("createD1Object")
})

setMethod("createD1Object", "D1Client", function(x, identifier, data, format, nodeId) {
   client <- x@client
   session <- x@session
   nodeurl <- "http://demo1.test.dataone.org/knb/d1/mn/v1"

   # Create identifier to be used in system metadata
   pid <- .jnew("org/dataone/service/types/v1/Identifier")
   pid$setValue(identifier)

   # Convert incoming data to byte array (byte[])
   ioUtils <-  .jnew("org/apache/commons/io/IOUtils") 
   byteArray <- ioUtils$toByteArray(data)

   # Set up/convert additional system metadata fields
   # get the submitter from the certificate
   certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
   cert <- certman$loadCertificate()
   submitter <- certman$getSubjectDN(cert)

   # Now create the object with the sysmeta values
   d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, format, submitter, nodeId, check=FALSE)
   if (!is.null(e<-.jgetEx())) {
       print("Java exception was raised")
       print(.jcheck(silent=TRUE))
       print(.jcheck(silent=TRUE))
       print(e)
   }

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

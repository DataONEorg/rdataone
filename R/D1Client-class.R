setClass("D1Client",
         representation(endpoint = "character",
                        client = "jobjRef",
                        session = "jobjRef")
)

#####################
## D1Client constructors
#####################

## generic
setGeneric("D1Client", function(...) { standardGeneric("D1Client")} )

## no arguments in the signature
setMethod("D1Client", ,
    function() {

    result <- new("D1Client", "PROD")

    ## create new D1Client object and insert uri endpoint
    result <- new("D1Client")
    
	## Create a Java D1Client object to use for contacting the server
    client <-  .jnew("org/dataone/client/D1Client") 
    result@client <- client
    result@session <-  .jnew("org/dataone/service/types/v1/Session") 
    return(result)
})

## Pass in the environment to be used by this D1Client
setMethod("D1Client", ,
    function(env) {

    ## Select which CN environment should be used
    PROD <- "https://cn.dataone.org/cn"
    STAGING <- "https://cn-stage.dataone.org/cn"
    SANDBOX <- "https://cn-sandbox.dataone.org/cn"
    DEV <- "https://cn-dev-rr.dataone.org/cn"
 
    CN_URI <- PROD
    if (env == "DEV") CN_URI <- DEV
    if (env == "STAGING") CN_URI <- STAGING
    if (env == "SANDBOX") CN_URI <- SANDBOX
    if (env == "PROD") CN_URI <- PROD
 
    config <- J("org/dataone/configuration/Settings")$getConfiguration()
    config$setProperty("D1Client.CN_URL", CN_URI)

    ## create new D1Client object and insert uri endpoint
    result <- new("D1Client")
    result@endpoint <- CN_URI
    
	## Create a Java D1Client object to use for contacting the server
    client <-  .jnew("org/dataone/client/D1Client") 
    result@client <- client
    result@session <-  .jnew("org/dataone/service/types/v1/Session") 
    return(result)
})

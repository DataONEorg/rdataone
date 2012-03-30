setClass("D1Client",
         representation(endpoint = "character",
                        username = "character",
                        client = "jobjRef",
                        session = "jobjRef")
)

#####################
## D1Client constructors
#####################

## generic
setGeneric("D1Client", function(...) { standardGeneric("D1Client")} )
#setGeneric("D1Client")

## no arguments in the signature
setMethod("D1Client", ,
    function() {

    ## create new D1Client object and insert uri endpoint
    result <- new("D1Client")
    #result@endpoint <- uri
    
	## Create a Java D1Client object to use for contacting the server
    client <-  .jnew("org/dataone/client/D1Client") 
    result@client <- client
    result@session <-  .jnew("org/dataone/service/types/v1/Session") 
    return(result)
})

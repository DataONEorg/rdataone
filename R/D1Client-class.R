setClass("D1Client",
         representation(endpoint = "character",
                        username = "character",
                        cli = "jobjRef",
                        token = "jobjRef")
)

#####################
## D1Client constructors
#####################

## generic
setGeneric("D1Client", function(uri, ...) { standardGeneric("D1Client")} )

## first arg is a character
setMethod("D1Client", "character",
    function(uri) {

    ## create new D1Client object and insert uri endpoint
    res <- new("D1Client")
    res@endpoint <- uri
    ## Create a Java D1Client object to use for contacting the server
    cli <-  .jnew("org/dataone/client/D1Client") 
    res@cli <- cli

    return(res)
})

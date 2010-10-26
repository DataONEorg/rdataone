setClass("DataPackage",
         representation(identifier = "character",
                        sysmeta = "jobjRef",
                        scimeta = "character",
			dataList = "list")
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(identifier, sysmeta1, scimeta1, ...) { standardGeneric("DataPackage")} )

## first arg is a character
setMethod("DataPackage", "character",
    function(identifier, sysmeta1, scimeta1) {

    ## create new DataPackage object and insert identifier and data
    res <- new("DataPackage")
    res@identifier <- identifier
    res@sysmeta <- sysmeta1
    res@scimeta <- scimeta1

    return(res)
})

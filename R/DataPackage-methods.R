## This file contains the methods and accessors for DataPackage objects

#########################################################
## Accessor methods
#########################################################

## addData to the end of the dataList
## takes the package and data object as input
setGeneric("addData", function(x, dataObject, ...) { 
    standardGeneric("addData")
})

setMethod("addData", "DataPackage", function(x, dataObject) {
   l <- length(x@dataList)
   x@dataList[l+1] <- list(dataObject)
   return(x)
})


## getData, returns data object at index
setGeneric("getData", function(x, index, ...) { standardGeneric("getData")} )

setMethod("getData", "DataPackage", function(x, index) {
    return(x@dataList[[index]])
})

## getDataCount, returns number of data objects in this package
setGeneric("getDataCount", function(x, ...) { standardGeneric("getDataCount")} )

setMethod("getDataCount", "DataPackage", function(x) {
    return(length(x@dataList))
})

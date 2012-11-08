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

## This file contains the methods and accessors for DataPackage objects



#########################################################
## Accessor methods
#########################################################

## addMeta adds another science metadata D1Object to the end of the meta slot
## takes the DataPackage and D1Object as input
setGeneric("addMeta", function(x, d1object, ...) { 
    standardGeneric("addMeta")
})

setMethod("addMeta", signature("DataPackage", "D1Object"), function(x, d1object) {
    if (!is.null(x@scimeta)) {
        l <- length(x@scimeta)
        x@scimeta[ l + 1 ] <- d1object
      } else {
          x@scimeta <- d1object
       }
})


## addData to the end of the dataList
## takes the package and data object as input
setGeneric("addData", function(x, dataObject, ...) { 
    standardGeneric("addData")
})

setMethod("addData", signature("DataPackage", "D1Object"),function(x, dataObject) {
 l <- length(x@dataList)
 x@dataList[ l + 1 ] <- list(dataObject)
})


## getData, returns data object at index
##setGeneric("getData", function(x, ...) { standardGeneric("getData")} )

setMethod("getData", signature("DataPackage", "numeric"), function(x, index) {

    d1Object <- x@dataList[[ index ]]
    if(!is.null(d1Object)) {
	databytes <- getData(d1Object)
	if(is.null(databytes)) {
	    print(paste("Didn't find data in object at index", index))
	    return
	}
	return(databytes)
    }
})

## getDataCount, returns number of data objects in this package
setGeneric("getDataCount", function(x, ...) { standardGeneric("getDataCount")} )

setMethod("getDataCount", "DataPackage", function(x) {
    return(length(x@dataList))
})

## getMetaCount, returns number of metadata objects in this package
setGeneric("getMetaCount", function(x, ...) { standardGeneric("getMetaCount")} )

setMethod("getMetaCount", "DataPackage", function(x) {
            return(length(x@scimeta))
        })



## getData, returns data object at index
setGeneric("asDataFrame", function(x, index, ...) { standardGeneric("asDataFrame")} )

setMethod("asDataFrame", "DataPackage", function(x, index) {
   # Load the data into a dataframe
   #df <- read.table(textConnection(x@dataList[[index]]), header = TRUE, sep = ",", na.strings = "-999")
   df <- read.csv(textConnection(x@dataList[[index]]))
   return(df)
})

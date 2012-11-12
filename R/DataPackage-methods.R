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



##  getData returns the actual data of an object contained in the package
##  given the identifier string
setMethod("getData", signature("DataPackage", "character"), function(x, id) {
  
  jIdentifier <- .jnew("org/dataone/service/types/v1/Identifier")
  jIdentifier$setValue(id)

  jD1Object <- x@jDataPackage$get(jIdentifier)
  if(!is.jnull(jD1Object)) {
    databytes <- jD1Object$getData()
    if(is.null(databytes)) {
      print(paste("Didn't find data in object", id))
      return()
    }
    return(databytes)
  }
})



## getDataCount, returns number of data objects in this package
setGeneric("getSize", function(x, ...) { standardGeneric("getSize")} )

setMethod("getSize", "DataPackage", function(x) {
  return(x@jDataPackage$size())
})

## getDataCount, returns number of data objects in this package
##setGeneric("getIdentifiers", function(x, ...) { standardGeneric("getIdentifiers")} )
##
##setMethod("getIdentifiers", "DataPackage", function(x) {
##  jSet <- x@jDataPackage$identifiers()
##  
##})



## addData adds a D1Object to the underlying Java DataPackage
## takes the DataPackage and rD1Object as input
setGeneric("addData", function(x, d1object, ...) { 
    standardGeneric("addData")
})

setMethod("addData", signature("DataPackage", "D1Object"), function(x, d1object) {
  jD1o <- d1object@d1o
  x@jDataPackage$addData(jD1o)
})

## addAndDownloadData downloads a D1Object to the DataPackage, using the provided identifier
## string to retrieve from the DataONE system.
## takes the DataPackage and a character string representing the identfier  as input
setGeneric("addAndDownloadData", function(x, identifier, ...) { 
    standardGeneric("addAndDownloadData")
})

setMethod("addAndDownloadData", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  x@jDataPackage$addAndDownloadData(jPid)
})



setGeneric("insertRelationship", function(x, metadataID, dataIDs, ...) {
  standardGeneric("insertRelationship")
})


setMethod("insertRelationship", signature("DataPackage", "character", "character"), function(x, metadataID, dataIDs) {
  jMetaPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jMetaPid$setValue(metadataID)
  
  jList <- .jnew("java/util/LinkedList")
  for (i in 1:length(dataIDs)) {
    jPid <- .jnew("org/dataone/service/types/v1/Identifier")
    jPid$setValue(dataIDs[i])
    jList$add(jPid)
  }
  x@jDataPackage$insertRelationship(jMetaPid, jList)
})


##
##
setGeneric("contains", function(x, identifier, ...) {
  standardGeneric("contains")
})


setMethod("contains", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  return(x@jDataPackage$contains(jPid))
})


##
##
setGeneric("remove", function(x, identifier, ...) {
  standardGeneric("remove")
})


setMethod("remove", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  x@jDataPackage$remove(jPid)
})

##
##
setGeneric("get", function(x, identifier, ...) {
  standardGeneric("get")
})


setMethod("get", signature("DataPackage", "character"), function(x, identifier) {
  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
  jPid$setValue(identifier)
  
  jD1Object <- x@jDataPackage$get(jPid)
  return(new(class = "D1Object", jD1o = jD1Object))
})




## getData, returns data object at index
setGeneric("asDataFrame", function(x, index, ...) { standardGeneric("asDataFrame")} )

setMethod("asDataFrame", "DataPackage", function(x, index) {
   # Load the data into a dataframe
   #df <- read.table(textConnection(x@dataList[[index]]), header = TRUE, sep = ",", na.strings = "-999")
   df <- read.csv(textConnection(x@dataList[[index]]))
   return(df)
})

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



#########################################################
### Utility methods
#########################################################

##
setGeneric("dataTable.entityNames", function(x, ...) {
    standardGeneric("dataTable.entityNames")
})
 
setMethod("dataTable.entityNames", signature("EMLParser"), function(x) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
    return(sapply(aList, function(x) xmlValue(x[["entityName"]])))
})



##
setGeneric("dataTable.distributionIdentification", function(x, ...) {
    standardGeneric("dataTable.distributionIdentification")
})

setMethod("dataTable.distributionIdentification", signature("EMLParser"), function(x) {
   aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/distribution/online")
   return(sapply(aList, function(x) xmlValue(x[["url"]])))
})



##
setGeneric("dataTable.size", function(x, ...) {
    standardGeneric("dataTable.size")
})

setMethod("dataTable.size", signature("EMLParser"), function(x) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
    return(sapply(aList, function(x) xmlValue(x[["size"]])))
})

##
setGeneric("dataTable.fieldDelimiter", function(x, ...) {
            standardGeneric("dataTable.fieldDelimiter")
        })

setMethod("dataTable.fieldDelimiter", signature("EMLParser"), function(x) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["fieldDelimiter"]])))
        })


##
setGeneric("dataTable.quoteCharacter", function(x, ...) {
            standardGeneric("dataTable.quoteCharacter")
        })

setMethod("dataTable.quoteCharacter", signature("EMLParser"), function(x) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["quoteCharacter"]])))
        })



##
setGeneric("dataTable.characterEncoding", function(x, ...) {
            standardGeneric("dataTable.characterEncoding")
        })

setMethod("dataTable.characterEncoding", signature("EMLParser"), function(x) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
            return(sapply(aList, function(x) xmlValue(x[["characterEncoding"]])))
        })


#########  EML-attribute items
##
setGeneric("dataTable.missingValueCodes", function(x, index, ...) {
            standardGeneric("dataTable.missingValueCodes")
        })

## 
setMethod("dataTable.missingValueCodes", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
            bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
                        
            return(sapply(bList, function(x) xmlValue(x[["missingValueCode"]])))
        })


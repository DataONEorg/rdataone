#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2013
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
### Interface methods (that follow the generics)
#########################################################


 
#' @name documented.entityNames
#' @alias documented.entityNames,-method
setMethod("documented.entityNames", signature("EMLParser"), function(x) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			return(sapply(aList, function(x) xmlValue(x[["entityName"]])))
		})



#' @name documented.d1Identifiers
#' @alias documented.d1Identifiers,-method
setMethod("documented.d1Identifiers", signature("EMLParser"), function(x) {
   aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/distribution/online")
   return(sapply(aList, function(x) { 
			gsub("ecogrid://knb/","",xmlValue(x[["url"]]))	   	   
		}))
})



#' @name documented.sizes
#' @alias documented.sizes,-method
setMethod("documented.sizes", signature("EMLParser"), function(x) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
    return(sapply(aList, function(x) xmlValue(x[["size"]])))
})




## @name data.formatFamily
## @alias data.formatFamily,-method
setMethod("data.formatFamily", signature("EMLParser", "numeric"), function(x, index) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
    bList <- getNodeSet(aList[[index]], "//dataTable/physical/dataFormat/textFormat/simpleDelimited")
    if (length(bList) == 1) { 
        format <- "text/simpleDelimited"
    } else {
        bList <- getNodeSet(aList[[index]], "//dataTable/physical/dataFormat/textFormat/complex")
        if (length(bList) == 1) {
            format <- "text/complex"
        } else {
            bList <- getNodeSet(aList[[index]], "//dataTable/physical/dataFormat/binaryRasterFormat")
            if (length(bList) == 1) {
                format <- "binary"
            } else {
                format <- "externallyDefined"
            }
        }
    }
    return(format)
})

setMethod("data.tableFieldDelimiter", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["fieldDelimiter"]])))
        })



setMethod("data.tableQuoteCharacter", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["quoteCharacter"]])))
        })




setMethod("data.characterEncoding", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
            return(sapply(aList, function(x) xmlValue(x[["characterEncoding"]])))
        })


 
setMethod("data.tableAttributeOrientation", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat")
			return( sapply(aList, function(x) xmlValue(x[["attributeOrientation"]]))[index])
		})


 
setMethod("data.tableSkipLinesHeader", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat")
			result <- sapply(aList, function(x) { 
						v <- xmlValue(x[["numHeaderLines"]])
						return(ifelse(v==0,0,as.numeric(v)-1))
					})
		})


#########  EML-attribute items
##
setMethod("data.tableMissingValueCodes", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
            bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
                        
            return(sapply(bList, function(x) xmlValue(x[["missingValueCode"]]$code)))
        })

##
setMethod("data.tableAttributeNames", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlValue(x[["attributeName"]])))
		})

##
setMethod("data.tableAttributeTypes", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlName(xmlChildren(x[["measurementScale"]])[[1]])))
		})


##
setMethod("data.tableAttributeStorageTypes", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlValue(x[["storageType"]])))
		})

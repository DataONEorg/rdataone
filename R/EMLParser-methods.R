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
setMethod("documents.d1FormatIds", signature("EMLParser"), function(x, ...) {
    return(c(
        "eml://ecoinformatics.org/eml-2.1.1", 
        "eml://ecoinformatics.org/eml-2.1.0",
        "eml://ecoinformatics.org/eml-2.0.1", 
        "eml://ecoinformatics.org/eml-2.0.0"
    ))
})



##
setMethod("documented.entityNames", signature("EMLParser"), function(x) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			return(sapply(aList, function(x) xmlValue(x[["entityName"]])))
		})

##
setMethod("documented.d1Identifiers", signature("EMLParser"), function(x) {
   aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/distribution/online")
   return(sapply(aList, function(x) { 
					   gsub("ecogrid://knb/","",xmlValue(x[["url"]]))
				   	   
				   }))
})


setMethod("documented.sizes", signature("EMLParser"), function(x) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
    return(sapply(aList, function(x) xmlValue(x[["size"]])))
})


setMethod("dataTable.dataFormat", signature("EMLParser", "numeric"), function(x, index) {
    aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
    bList <- getNodeSet(aList[[index]], "//dataTable/dataFormat/textFormat/simpleDelimited")
    if (length(bList) == 1) { 
        format <- "text/simpleDelimited"
    } else {
        bList <- getNodeSet(aList[[index]], "//dataTable/dataFormat/textFormat/complex")
        if (length(bList) == 1) {
            format <- "text/complex"
        } else {
            bList <- getNodeSet(aList[[index]], "//dataTable/dataFormat/binaryRasterFormat")
            if (length(bList) == 1) {
                format <- "binary"
            } else {
                format <- "externallyDefined"
            }
        }
    }
    return(format)
})

setMethod("dataTable.fieldDelimiter", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["fieldDelimiter"]])))
        })



setMethod("dataTable.quoteCharacter", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited")
            return(sapply(aList, function(x) xmlValue(x[["quoteCharacter"]])))
        })




setMethod("dataTable.characterEncoding", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical")
            return(sapply(aList, function(x) xmlValue(x[["characterEncoding"]])))
        })


 
setMethod("dataTable.attributeOrientation", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat")
			return( sapply(aList, function(x) xmlValue(x[["attributeOrientation"]]))[index])
		})


 
setMethod("dataTable.skipLinesHeader", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable/physical/dataFormat/textFormat")
			return(sapply(aList, function(x) xmlValue(x[["numHeaderLines"]])))
		})


#########  EML-attribute items
##
setMethod("dataTable.missingValueCodes", signature("EMLParser", "numeric"), function(x, index) {
            aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
            bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
                        
            return(sapply(bList, function(x) xmlValue(x[["missingValueCode"]]$code)))
        })

##
setMethod("dataTable.attributeNames", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlValue(x[["attributeName"]])))
		})

##
setMethod("dataTable.attributeTypes", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlName(xmlChildren(x[["measurementScale"]])[[1]])))
		})


##
setMethod("dataTable.attributeStorageTypes", signature("EMLParser", "numeric"), function(x, index) {
			aList <- getNodeSet(x@xmlDocRoot,"//dataset/dataTable")
			bList <- getNodeSet(aList[[index]],"//attributeList/attribute")
			
			return(sapply(bList, function(x) xmlValue(x[["storageType"]])))
		})

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

setClass("DataTableDescriber", representation=representation(dataoneFormatIds = "character"))

## create a global where implementing classes will map the formats they handle
## in the form:
##     dataTableDescriber.registry[[ <d1FormatId> ]] <- <implementing class>

if (!exists("dataTableDescriber.registry")) dataTableDescriber.registry <- list()


##########################
## 
##########################

## generic
setGeneric("documents.d1FormatIds", function(x, ...) {
    standardGeneric("documents.d1FormatIds")
})

##
setGeneric("documented.entityNames", function(x, ...) {
    standardGeneric("documented.entityNames")
})

##
setGeneric("documented.d1Identifiers", function(x, ...) {
    standardGeneric("documented.d1Identifiers")
})

##
setGeneric("documented.sizes", function(x, ...) {
    standardGeneric("documented.sizes")
})

##
setGeneric("dataTable.dataFormat", function(x, index, ...) {
    standardGeneric("dataTable.dataFormat")
})

##
setGeneric("dataTable.fieldDelimiter", function(x, index, ...) {
    standardGeneric("dataTable.fieldDelimiter")
})

##
setGeneric("dataTable.quoteCharacter", function(x, index, ...) {
    standardGeneric("dataTable.quoteCharacter")
})

##
setGeneric("dataTable.characterEncoding", function(x, index, ...) {
    standardGeneric("dataTable.characterEncoding")
})


#' returns whether the data table has attributes arranged in 'columns' or 'rows'
#' @param x - the DataTableDescriber instance
#' @param index - the index of the dataTable to get results for
#' @param ... 
#' @returnType character
#' @return columns | rows
#' 
#' @author rnahf
#' @export
setGeneric("dataTable.attributeOrientation", function(x, index, ...) {
    standardGeneric("dataTable.attributeOrientation")
})

##
setGeneric("dataTable.skipLinesHeader", function(x, index, ...) {
    standardGeneric("dataTable.skipLinesHeader")
})

#########  EML-attribute items

#' returns the missing value codes defined in the metadata document for 
#' the specified data table
#' @param x - the DataTableDescriber instance
#' @param index - the index of the dataTable to get results for
#' @param ... 
#' @returnType character
#' @return vector of missing value codes
#' 
#' @author rnahf
#' @export
setGeneric("dataTable.missingValueCodes", function(x, index, ...) {
    standardGeneric("dataTable.missingValueCodes")
})


#' returns the attribute names defined in the metadata document for 
#' the specified data table
#' @param x - the DataTableDescriber instance
#' @param index - the index of the dataTable to get results for
#' @param ... 
#' @returnType character
#' @return the attribute (column) names of the data
#' 
#' @author rnahf
#' @export
setGeneric("dataTable.attributeNames", function(x, index, ...) {
    standardGeneric("dataTable.attributeNames")
})


#' returns the attributes' data types defined in the metadata document for 
#' the specified data table
#' @param x - the DataTableDescriber instance
#' @param index - the index of the dataTable to get results for
#' @param ... 
#' @returnType character
#' @return the data types of the attributes
#' 
#' @author rnahf
#' @export
setGeneric("dataTable.attributeTypes", function(x, index, ...) {
    standardGeneric("dataTable.attributeTypes")
})

#' returns the attributes' data storage types defined in the metadata document for 
#' the specified data table
#' @param x - the DataTableDescriber instance
#' @param index - the index of the dataTable to get results for
#' @param ... 
#' @returnType character
#' @return the data storage types of the attributes
#' 
#' @author rnahf
#' @export
setGeneric("dataTable.attributeStorageTypes", function(x, index, ...) {
    standardGeneric("dataTable.attributeStorageTypes")
})


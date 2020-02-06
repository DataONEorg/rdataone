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

#' @title Base Class for Specific Metadata Parsers
#' @description Classes that inherit from this class provide the format-specific ways to
#' provide read.csv with parsing instructions.
#' @details This class defines the generic methods metadata parser classes need to implement
#' to allow proper parsing of tabular data objects.  Subclasses should:
#' 1. provide method implementations for all generics
#' 2. register the class to the tableDescriber.registry for the formats they claim to parse.
#' 3. provide a 'constructor' method that accepts a D1Object as the first argument - the 
#' D1Object will be the metadata object to be parsed
#' 
#' For example, the EMLParser registers itself as a handler for eml v2.0.0 - v2.1.1
#' with the following.
#' 
#' \code{
#' if (!exists("tableDescriber.registry")) tableDescriber.registry <- list()
#' tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.0" ]] <- "EMLParser"
#' tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.1" ]] <- "EMLParser"
#' tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.0" ]] <- "EMLParser"
#' tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.1" ]] <- "EMLParser"
#' }
#' 
#' Note that the key in the list is the DataONE formatIdentifier that can be 
#' found at "https://cn.dataone.org/cn/v2/formats".
#' 
#' Subclass implementers should conform their methods to the behavior defined 
#' in the generic.
#' @rdname AbstractTableDescriber-class
#' @aliases AbstractTableDescriber-class
#' @author rnahf
#' @export
##setClass("AbstractTableDescriber", representation=representation(dataoneFormatIds = "character"))
setClass("AbstractTableDescriber")

## create a global where implementing classes will map the formats they handle
## in the form:
##     tableDescriber.registry[[ <d1FormatId> ]] <- <implementing class>

if (!exists("tableDescriber.registry")) tableDescriber.registry <- list()


##########################
## 
##########################


## Get the DataONE formatIdentifier associated with each table
## 
## @param x the TableDescriber
## @param ... 
## @returnType character
## @return vector of DataONE formatIds
## 
## @author rnahf
## @export
##setGeneric("documented.d1FormatIds", function(x, ...) {
##    standardGeneric("documented.d1FormatIds")
##})
## see formatFamily generic instead.

#' @title Get the entity names associated with each table
#' @description The entity names associated with each table are returned.
#' @rdname documented.entityNames
#' @param x the TableDescriber
#' @param ...  Additional parameters
#' @return vector of entity names
#' @author rnahf
#' @export
setGeneric("documented.entityNames", function(x, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("documented.entityNames")
})

#' Get DataONE identifiers
#' @description Get the DataONE identifier associated with each table
#' @rdname documented.d1Identifiers
#' @param x the TableDescriber
#' @param ... Additional parameters
#' @return vector of dataONE identifiers
#' @author rnahf
#' @export
setGeneric("documented.d1Identifiers", function(x, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("documented.d1Identifiers")
})

#' @title Get the sizes of the described data tables.
#' @description Get the table size.
#' @rdname documented.sizes
#' @param x the TableDescriber
#' @param ... Additional parameters
#' @return vector of data table sizes (in bytes)
#' @author rnahf
#' @export
setGeneric("documented.sizes", function(x, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("documented.sizes")
})

#' @title Data Format
#' @description Get the table format family.
#' @rdname data.formatFamily
#' @param x the TableDescriber
#' @param index index of the table within the document
#' @param ... Additional parameters
#' @return the format of the data object being described
#' @author rnahf
#' @export
setGeneric("data.formatFamily", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.formatFamily")
})

#' Field Delimiter
#' @description Get the table field delimiter.
#' @rdname data.tableFieldDelimiter
#' @param x the TableDescriber
#' @param index index of the table within the document
#' @param ... Additional parameters
#' @return the field delimiter(s) of the data object being described
#' @author rnahf
#' @export
setGeneric("data.tableFieldDelimiter", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableFieldDelimiter")
})

#' @title Quote Character
#' @description Get the table quote character.
#' @rdname data.tableQuoteCharacter
#' @param x the TableDescriber
#' @param index index of the table within the document
#' @param ... Additional parameters
#' @return the quote characters(s) for the data object being described
#' @author rnahf
#' @export
setGeneric("data.tableQuoteCharacter", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableQuoteCharacter")
})

#' @title CharacterEncoding
#' @description The character encoding used, for example "UTF-8"
#' @rdname data.characterEncoding
#' @param x the TableDescriber
#' @param index index of the table within the document
#' @param ... Additional parameters
#' @return the encoding used when serializing the data
#' @author rnahf
#' @export
setGeneric("data.characterEncoding", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.characterEncoding")
})

#' @title The Attribute (Header) Orientation
#' @description Which way to the attribute headers run?  Most data has a header row
#' where the attribute names go across "columns", in row in which case, the 
#' return value for this method should be "columns."
#' @note this is the opposite question from how records are organized!!
#' @rdname data.tableAttributeOrientation
#' @param x - the TableDescriber 
#' @param index - the index of the table within the document
#' @param ... Additional parameters
#' @return legal values  are "columns" | "rows"
#' @author rnahf
#' @export
setGeneric("data.tableAttributeOrientation", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableAttributeOrientation")
})

#' Number of lines to skip before reading data
#' @description The specified number of lines are skipped.
#' @rdname data.tableSkipLinesHeader
#' @param x - the TableDescriber 
#' @param index - the index of the table within the document
#' @param ... Additional parameters
#' @return the number of lines to skip
#' @seealso \code{help(read.table)}
#' @author rnahf
#' @export
setGeneric("data.tableSkipLinesHeader", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableSkipLinesHeader")
})

#########  EML-attribute items

#' @title returns missing value codes 
#' @description the missing value codes are defined in the metadata document for 
#' the specified data table
#' @rdname data.tableMissingValueCodes
#' @param x - the TableDescriber instance
#' @param index - the index of the table to get results for
#' @param ... (not yet used)
#' @return vector of missing value codes
#' @author rnahf
#' @export
setGeneric("data.tableMissingValueCodes", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableMissingValueCodes")
})

#' @title returns the attribute names 
#' @description THe attribute names are defined in the metadata document for 
#' the specified data table
#' @rdname data.tableAttributeNames
#' @param x - the TableDescriber instance
#' @param index - the index of the table to get results for
#' @param ... (not yet used)
#' @return the attribute (column) names of the data
#' @author rnahf
#' @export
setGeneric("data.tableAttributeNames", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableAttributeNames")
})

#' @title returns the attributes' data types 
#' @description The attributes' data types are defined in the metadata document for 
#' the specified data table
#' @rdname data.tableAttributeTypes
#' @param x - the TableDescriber instance
#' @param index - the index of the table to get results for
#' @param ... (not yet used)
#' @return the data types of the attributes
#' @author rnahf
#' @export
setGeneric("data.tableAttributeTypes", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableAttributeTypes")
})

#' @title returns the attributes' data storage types 
#' @description The attributes' data storage types are defined in the metadata document for 
#' the specified data table
#' @rdname data.tableAttributeStorageTypes
#' @param x - the TableDescriber instance
#' @param index - the index of the table to get results for
#' @param ... (not yet used)
#' @return the data storage types of the attributes
#' @author rnahf
#' @export
setGeneric("data.tableAttributeStorageTypes", function(x, index, ...) {
  .Defunct("eml_get", "EML")
    standardGeneric("data.tableAttributeStorageTypes")
})

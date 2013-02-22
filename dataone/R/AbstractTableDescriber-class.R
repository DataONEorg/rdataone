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


## Base Class for Specific Metadata Parsers
## 
## Classes that inherit from this class provide the format-specific ways to
## provide read.csv with parsing instructions.
## 
## @details 
## This class defines the generic methods metadata parser classes need to implement
## to allow proper parsing of tabular data objects.  Subclasses should:
## 1. provide method implementations for all generics
## 2. register the class to the tableDescriber.registry for the formats they claim to parse.
## 3. provide a 'constructor' method that accepts a D1Object as the first argument - the 
## D1Object will be the metadata object to be parsed
## 
## For example, the EMLParser registers itself as a handler for eml v2.0.0 - v2.1.1
## with the following.
## 
## \code{
## if (!exists("tableDescriber.registry")) tableDescriber.registry <- list()
## tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.0" ]] <- "EMLParser"
## tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.1" ]] <- "EMLParser"
## tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.0" ]] <- "EMLParser"
## tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.1" ]] <- "EMLParser"
## }
## 
## Note that the key in the list is the DataONE formatIdentifier that can be 
## found at \link{https://cn.dataone.org/cn/v1/formats}
## 
## Subclass implementers should conform their methods to the behavior defined 
## in the generic.
## 
## @examples
## \dontrun{
##    ## asDataFrame(D1Object,D1Object) implementation uses the following:
##    dtdClassName <- tableDescriber.registry[[ metadataFormatId ]]
##    dtd <- do.call(dtdClassName, list(metadata.d1Object))
## }
##  
## @author rnahf
## @export
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
## @return vector of dataONE formatIds
## 
## @author rnahf
## @export
##setGeneric("documented.d1FormatIds", function(x, ...) {
##    standardGeneric("documented.d1FormatIds")
##})
## see formatFamily generic instead.


## Get the entity names associated with each table
## 
## @param x the TableDescriber
## @param ... 
## @returnType character
## @return vector of entity names
## 
## @author rnahf
## @export
setGeneric("documented.entityNames", function(x, ...) {
    standardGeneric("documented.entityNames")
})


## Get the DataONE identifiers associated with each table
## 
## @param x the TableDescriber
## @param ... 
## @returnType character
## @return vector of dataONE identifiers
## 
## @author rnahf
## @export
setGeneric("documented.d1Identifiers", function(x, ...) {
    standardGeneric("documented.d1Identifiers")
})


## Get the sizes of the described data tables.
## 
## @param x the TableDescriber
## @param ... 
## @returnType character
## @return vector of data table sizes (in bytes)
## 
## @author rnahf
## @export
setGeneric("documented.sizes", function(x, ...) {
    standardGeneric("documented.sizes")
})


## Data Format
## 
## @param x the TableDescriber
## @param index index of the table within the document
## @param ... 
## @returnType character
## @return the format of the data object being described
## TODO: list valid values
## 
## @author rnahf
## @export
setGeneric("data.formatFamily", function(x, index, ...) {
    standardGeneric("data.formatFamily")
})


## Field Delimiter
## 
## @param x the TableDescriber
## @param index index of the table within the document
## @param ... 
## @returnType character
## @return the field delimiter(s) of the data object being described
## 
## @author rnahf
## @export
setGeneric("data.tableFieldDelimiter", function(x, index, ...) {
    standardGeneric("data.tableFieldDelimiter")
})


## Quote Character
## 
## @param x the TableDescriber
## @param index index of the table within the document
## @param ... 
## @returnType character
## @return the quote characters(s) for the data object being described
## 
## @author rnahf
## @export
setGeneric("data.tableQuoteCharacter", function(x, index, ...) {
    standardGeneric("data.tableQuoteCharacter")
})


## CharacterEncoding
## 
## For example "UTF-8"
## 
## @param x the TableDescriber
## @param index index of the table within the document
## @param ... 
## @returnType character
## @return the encoding used when serializing the data
## 
## @author rnahf
## @export
setGeneric("data.characterEncoding", function(x, index, ...) {
    standardGeneric("data.characterEncoding")
})


## The Attribute (Header) Orientation
## 
## Which way to the attribute headers run?  Most data has a header row
## where the attribute names go across "columns", in row in which case, the 
## return value for this method should be "columns."
## 
## @note this is the opposite question from how records are organized!!
## @param x - the TableDescriber 
## @param index - the index of the table within the document
## @param ... 
## @returnType character
## @return legal values  are "columns" | "rows"
## 
## @author rnahf
## @export
setGeneric("data.tableAttributeOrientation", function(x, index, ...) {
    standardGeneric("data.tableAttributeOrientation")
})


## TODO - does this include the header line or not? what does value of 1 mean?, 0?
## Number of lines to skip before reading data
## 
## @param x - the TableDescriber 
## @param index - the index of the table within the document
## @param ... 
## @returnType numeric
## @return the number of lines to skip
## @seealso \code{help(read.table)}
## @author rnahf
## @export
setGeneric("data.tableSkipLinesHeader", function(x, index, ...) {
    standardGeneric("data.tableSkipLinesHeader")
})

#########  EML-attribute items

## returns the missing value codes defined in the metadata document for 
## the specified data table
## @param x - the TableDescriber instance
## @param index - the index of the table to get results for
## @param ... (not yet used)
## @returnType character
## @return vector of missing value codes
## 
## @author rnahf
## @export
setGeneric("data.tableMissingValueCodes", function(x, index, ...) {
    standardGeneric("data.tableMissingValueCodes")
})


## returns the attribute names defined in the metadata document for 
## the specified data table
## @param x - the TableDescriber instance
## @param index - the index of the table to get results for
## @param ... (not yet used)
## @returnType character
## @return the attribute (column) names of the data
## 
## @author rnahf
## @export
setGeneric("data.tableAttributeNames", function(x, index, ...) {
    standardGeneric("data.tableAttributeNames")
})


## returns the attributes' data types defined in the metadata document for 
## the specified data table
## @param x - the TableDescriber instance
## @param index - the index of the table to get results for
## @param ... (not yet used)
## @returnType character
## @return the data types of the attributes
## 
## @author rnahf
## @export
setGeneric("data.tableAttributeTypes", function(x, index, ...) {
    standardGeneric("data.tableAttributeTypes")
})

## returns the attributes' data storage types defined in the metadata document for 
## the specified data table
## @param x - the TableDescriber instance
## @param index - the index of the table to get results for
## @param ... (not yet used)
## @returnType character
## @return the data storage types of the attributes
## 
## @author rnahf
## @export
setGeneric("data.tableAttributeStorageTypes", function(x, index, ...) {
    standardGeneric("data.tableAttributeStorageTypes")
})


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
#
# Author: rnahf
###############################################################################



## TODO: turn these back into roxygen comments when roxygen3 becomes available
## (roxygen2 doesn't handle overloaded methods well)  (roxygen comments start
## with pound-singlequote)
##
## Build a DataFrame From a Data Object
## 
## The generic asDataFrame function returns the content of a DataONE object as 
## a DataFrame, assuming that the object represents scientific data and can be 
## coerced into tabular form.  Several methods exist to accomplish this.
## 
## 
## @details 
## In many cases, a science metadata object includes in its description how the 
## scientific data is stored, and this information should be used when converting
## from tabular form to a dataFrame.  The 2-argument implementations for asDataFrame
## exist to allow this to happen.  
## 
## Most users will use the implementation that takes the DataPackage and the member
## identifier of the data object to read into a dataFrame.  In this case, the 
## the dataPackage isolates the appropriate science metadata object, finds the 
## appropriate TableDescriber for it, and passes it to the single-argument
## \code{asDataFrame(D1Object,...)} method.
## 
## The other double-argument methods are progressively more direct, and are most
## useful when there is not a DataPackage that can associate the metadata to the
## data.  
## 
## The TableDescriber class is abstract, with data-format-specific subclasses
## providing the specific logic for parsing that particular data format (for example
## EMLParser for reading and parsing EML documents).  If there is no metadata parser
## for the metadata that describes the object, you will have to determine the 
## correct parameters for read.csv manually.
##  
## @note
## As of Feb 2013, there is only a metadata parser for EML documents.  Other parsers
## should become available as separate packages that can be loaded separately. 
## 
## 
## 
## @param x : of type DataPackage or D1Object. Where the object is.
## @param reference : either the identifier for the data, the D1Object of the metadata
## that describes it, the TableDescriber object, or missing
## @param ... : overriding parameters for read.csv
## 
## @returnType dataFrame
## @return a dataFrame representation of the object's data
## 
## @author rnahf
## @export
## 
## @examples 
##   \dontrun{
## df <- asDataFrame(data.package, dataMember.id)
## 
## df <- asDataFrame(data.object, its.metadata)
## 
## table.describer <- EMLParser(its.metadata)
## df <- asDataFrame(data.object, table.describer)
## 
## df <- asDataFrame(data.object, sep="\t", ...)
## 
## df <- asDataFrame(data.object)
## 
##   }
## 
## @seealso \code{\link{read.csv}} 
## 
## @docType methods
## @rdname asDataFrame-methods
setGeneric("asDataFrame", function(x, reference, ...) { 
    standardGeneric("asDataFrame")
})

## returns a DataFrame from the specified data object
## 
## Given the identifier of a member of the data package, return a data frame
## using any parsing instructions contained in its describing science metadata
## 
## @rdname asDataFrame-methods
## @aliases asDataFrame,DataPackage,character-method
setMethod("asDataFrame", signature("DataPackage", "character"), function(x, reference) {
			
			## find the dataObject and the metadata that Documents it
			d1o <- getMember(x,reference)
			jMetadataId <- x@jDataPackage$getDocumentedBy(d1o@jD1o$getIdentifier())
			documenterObject <- getMember(x,jMetadataId$getValue())
			message("@@ asDataFrame / DP")
			##	message(documenterObject)
			df <- asDataFrame(d1o,documenterObject)
			return(df)
		})

##
## this method uses the provided metadata reference object for instructions on
## how to parse the data table (which parameters to set)
## 'reference' is the metadata D1Object that gives instruction on how to read the data
## into the dataFrame
##
## @rdname asDataFrame-methods
## aliases asDataFrame,D1Object,D1Object-method
setMethod("asDataFrame", signature("D1Object", "D1Object"), function(x, reference, ...) {
			## reference is a metadata D1Object
			mdFormat <- getFormatId(reference)
			
			dtdClassName <- tableDescriber.registry[[ mdFormat ]]
			message(paste("@@ asDataFrame/Object", getIdentifier(reference), dtdClassName))
			if (!is.na(dtdClassName)) {
				dtd <-	do.call(dtdClassName, list(reference))
				df <- asDataFrame(x,dtd)
			} else {
				print("Could not find metadata parser, trying as plain csv...")
				df <-  asDataFrame(x)
			}
			return( df )
		})


## @rdname asDataFrame-methods
## aliases asDataFrame,D1Object,AbstractTableDescriber
setMethod("asDataFrame", signature("D1Object", "AbstractTableDescriber"), function(x, reference, ...) {
			
			message("asDataFrame / D1Object-dtd",class(reference))
			## reference is a TableDescriber
			pids <- documented.d1Identifiers(reference)
			jDataId <- x@jD1o$getIdentifier()$getValue()
			index <- which(pids == jDataId)
			message(paste("Index of data item is",index))
			
			## is this a datatype that we can handle?
			dataFormat <- data.formatFamily(reference,index)
			if (dataFormat != "text/simpleDelimited") {
				print(paste("cannot process data of type", dataFormat))
				return()
			} else if (data.tableAttributeOrientation(reference, index) == 'row') {
				print(paste("cannot process text/simpleDelimited file where attributes are by row"))
			}
			
			fieldSeparator <- data.tableFieldDelimiter(reference, index)
			if (is.na(fieldSeparator))
				fieldSeparator <- ","
			
			quoteChar <- data.tableQuoteCharacter(reference, index)
			if (is.na(quoteChar))
				quotChar <- "\""
			
			missingValues <- data.tableMissingValueCodes(reference,index)
			missingValues <- subset(missingValues, !is.na(missingValues))
			if(length(missingValues)==0)
				missingValues <- "NA"
			
			encoding <- data.characterEncoding(reference, index)
			if (is.na(encoding))
				encoding <- "unknown"
			
			skip <- data.tableSkipLinesHeader(reference, index)
			if (is.na(skip))
				skip <- 0
			
			
			## TODO: add the colClasses logic
			
			## as.is = !stringsAsFactors,
			##  colClasses = NA, nrows = -1,
			## check.names = TRUE, 
			## fill = !blank.lines.skip,
			## strip.white = FALSE, 
			## blank.lines.skip = TRUE,
			## comment.char = "#",
			## allowEscapes = FALSE, flush = FALSE,
			## stringsAsFactors = default.stringsAsFactors(),
			message("@@ skip ",skip)
			message("@@ sep ",fieldSeparator)
			message("@@ quote ",quoteChar)
			message("@@ na.strings ",missingValues)
			message("@@ encoding ",encoding)
			df <- asDataFrame(x, skip=skip, header=TRUE, sep=fieldSeparator, quote=quoteChar, 
					na.strings=missingValues, encoding=encoding)
			return(df)
		})

##
##  this method performs a read.csv on the D1Object data.  As with read.csv, you 
##  can use any of the parameters from read.table to override default behavior 
##  (see read.csv and read.table)
##
## @rdname asDataFrame-methods
## aliases asDataFrame,D1Object,ANY-method
setMethod("asDataFrame", signature("D1Object"), function(x, ...) {
			## Load the data into a dataframe
			
			dataBytes <- getData(x)
			theData <- textConnection(dataBytes)
			message("theData is ", class(theData))
			## using read.csv instead of read.table, because it exposes the defaults we want
			## while also allowing them to be overriden
			df <- read.csv(theData, ...)
			return(df)
		})

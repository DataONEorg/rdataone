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

#' D1Object (Deprecated) is a representation of a DataObject.
#' @description D1Object has been deprecated in favor of datapackage::DataObject, which provides
#' a wrapper for data and associated SystemMetadata. 
#' @slot dataObject A backing instance of a DataObject, to which all methods and state are proxied
#' @author Matthew Jones
#' @keywords classes
#' @import datapackage
setClass("D1Object", slots = c(proxyObject="DataObject") )

########################
## D1Object constructors
########################

## generic
setGeneric("D1Object", function(...) { standardGeneric("D1Object")} )

setMethod("initialize", "D1Object", function(.Object, id=as.character(NA), data=NA, 
                                             format=as.character(NA), mnNodeId=as.character(NA), 
                                             filename=as.character(NA)) {
  .Deprecated("DataObject", package="dataone")
  if (typeof(id) == "character") {
  } else {
  }
  
  .Object@dataObject <- new("DataObject", id = id, dataObj = data, format = format,
                            mnNodeId = mnNodeId, filename = filename)
  return(.Object)
})


#########################################################
### MNRead and MNStorage methods
#########################################################


#########################################################
### Utility methods
#########################################################

#' Get the Contents of the Specified Data Object
#' 
#' @param x  D1Object: the object from which to retrieve bytes of data
#' @param ... (not yet used)
#' @return character representation of the data
#' 
#' @author rnahf
#' @export
setMethod("getData", signature("D1Object"), function(x, id) {
    data <- get(x@mn, pid, check=as.logical(FALSE))
})

#' Get the Identifier of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @return the identifier
#' 
#' @author rnahf
#' @export
setMethod("getIdentifier", signature("D1Object"), function(x) {
  getIdentifier(x@dataObject)
})

#' Get the FormatId of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @return the formatId
#' 
#' @author rnahf
#' @export
setMethod("getFormatId", signature("D1Object"), function(x) {
  getFormatId(x@dataObject)
})

#' Add a Rule to the AccessPolicy to Make the Object Publicly Readable
#' 
#' To be called prior to creating the object in DataONE.  When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any affect. 
#' @param x D1Object
#' @param ... (not yet used)
#' @return NULL
#' 
#' @author rnahf
#' @export
setMethod("setPublicAccess", signature("D1Object"), function(x) {
  setPublicAccess(x@dataObject)
})

#' Test whether the provided subject can read the object
#' 
#' Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission, so uses
#' only the AccessPolicy to determine who can read (Not the rightsHolder field,
#' which always can read.)
#' @param x D1Client
#' @param subject A character containing the subject to check read access for.
#' @param ... (not yet used)
#' @return A logical value
#' @author rnahf
#' @export
setMethod("canRead", signature("D1Object", "character"), function(x, subject) {
  canRead(x@dataObject, subject)
})

setGeneric("asDataFrame", function(x, reference, ...) { 
            standardGeneric("asDataFrame")
})

#' 
#' this method uses the provided metadata reference object for instructions on
#' how to parse the data table (which parameters to set)
#' 'reference' is the metadata D1Object that gives instruction on how to read the data
#' into the dataFrame
#'
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
			## trust the metadata, not the d1FormatId of the object
            dataFormat <- data.formatFamily(reference,index)
            if (dataFormat != "text/simpleDelimited") {
				message("cannot process data of type", dataFormat)
                return()
            } else if (data.tableAttributeOrientation(reference, index) == 'row') {
				message("cannot process text/simpleDelimited file where attributes are by row")
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



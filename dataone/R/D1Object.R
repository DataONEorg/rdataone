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
#' @rdname D1Object-class
#' @aliases D1Object-class
#' @keywords classes
#' @import datapackage
#' @export
setClass("D1Object", slots = c(dataObject="DataObject") )

########################
## D1Object constructors
########################

#
#' Create a D1Object instance.
#' @param ... (additional arguments)
#' @rdname D1Object
#' @aliases D1Object
#' @return the D1Object instance
#' @seealso \code{\link[=D1Object-class]{D1Object}}{ class description.}
#' @export
setGeneric("D1Object", function(...) { standardGeneric("D1Object")} )

#' Initialize a D1Object
#' @param .Object A D1Object instance.
#' @param id The identifier for the object
#' @param data An R object (data or metadata) that this D1Object contains.
#' @param format The Object format.
#' @param mnNodeId The DataONE node identifier associated with this object, i.e. "urn:node:KNB"
#' @param filename A filename that this D1Object contains.
#' @rdname D1Object-initialize
#' @aliases D1Object-initialize
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

#' Get the data content of a specified data object
#' 
#' @param x  D1Object the data structure from where to get the data
#' @param id Missing or character: if \code{'x'} is DataPackage, the identifier of the package member to get data from
#' @describeIn D1Object
#' @export
setMethod("getData", signature("D1Object"), function(x, id) {
    data <- get(x@mn, id, check=as.logical(FALSE))
})

#' Get the Identifier of the DataObject
#' @param x D1Object
#' @param ... (not yet used)
#' @describeIn D1Object
#' @return the identifier
#' @export
setMethod("getIdentifier", signature("D1Object"), function(x) {
  getIdentifier(x@dataObject)
})

#' Get the FormatId of the DataObject
#' @param x D1Object
#' @param ... (not yet used)
#' @return the formatId
#' @describeIn D1Object
#' @export
setMethod("getFormatId", signature("D1Object"), function(x) {
  getFormatId(x@dataObject)
})

#' Add a Rule to the AccessPolicy to make the object publicly readable.
#' @description To be called prior to creating the object in DataONE.  When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any effect on remotely uploaded copies of
#' the D1Object. 
#' @param x D1Object
#' @param ... (not yet used)
#' @return D1Object with modified access rules
#' @seealso \code{\link[=DataObject-class]{DataObject}}{ class description.}
#' @describeIn D1Object
#' @export
setMethod("setPublicAccess", signature("D1Object"), function(x) {
  setPublicAccess(x@dataObject)
})

#' Test whether the provided subject can read an object.
#' @description Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission to a repository, 
#' and will show the permissions that would be enfirced by the repository on submission.
#' Currently it only uses the AccessPolicy to determine who can read (and not the rightsHolder field,
#' which always can read an object).  If an object has been granted read access by the
#' special "public" subject, then all subjects have read access.
#' @details The subject name used in both the AccessPolicy and in the \code{'subject'}
#' argument to this method is a string value, but is generally formatted as an X.509
#' name formatted according to RFC 2253.
#' @param x D1Object
#' @param subject : the subject name of the person/system to check for read permissions
#' @param ... Additional arguments
#' @return boolean TRUE if the subject has read permission, or FALSE otherwise
#' @describeIn D1Object
#' @export
setMethod("canRead", signature("D1Object", "character"), function(x, subject) {
  canRead(x@dataObject, subject)
})

#' This method uses the provided metadata reference object for instructions on
#' how to parse the data table (which parameters to set)
#' 'reference' is the metadata D1Object that gives instruction on how to read the data
#' into the dataFrame
#' @param x A D1Object
#' @param reference A reference to a D1Object
#' @param ... (Additional parameters)
#' @export
setGeneric("asDataFrame", function(x, reference, ...) { 
            standardGeneric("asDataFrame")
})

#' @describeIn asDataFrame
#' @export
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

#' @describeIn asDataFrame
#' @export
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

#' @describeIn asDataFrame
#' @export
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

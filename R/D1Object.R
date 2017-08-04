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
#' @description D1Object has been deprecated in favor of datapack::DataObject, which provides
#' a wrapper for data and associated SystemMetadata. 
#' @slot dataObject A backing instance of a DataObject, to which all methods and state are proxied
#' @rdname D1Object-class
#' @aliases D1Object-class
#' @keywords classes
#' @import datapack
#' @importFrom utils read.csv
#' @section Methods:
#' \itemize{
#'  \item{\code{\link[dataone]{D1Object-initialize}}}{: Initialize a D1Object}
#'  \item{\code{\link[dataone]{getData}}}{: Get the data content of a specified D1Object.}
#'  \item{\code{\link[dataone]{getIdentifier}}}{: Get the identifier of the D1Object.}
#'  \item{\code{\link[dataone]{getFormatId}}}{: Get the formatId of the D1Object}
#'  \item{\code{\link[dataone]{setPublicAccess}}}{: Add a Rule to the AccessPolicy to make the object publicly readable.}
#'  \item{\code{\link[dataone]{canRead}}}{: Test whether the provided subject can read an object.}
#'  \item{\code{\link{asDataFrame}}}{: Return the D1Object as a data.frame.}
#' }
#' @seealso \code{\link{dataone}}{ package description.}
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
setGeneric("D1Object", function(...) { 
  standardGeneric("D1Object") 
})

#' Initialize a D1Object
#' @param .Object A D1Object instance.
#' @param id The identifier for the object
#' @param data An R object (data or metadata) that this D1Object contains.
#' @param format The Object format.
#' @param mnNodeId The DataONE node identifier associated with this object, i.e. "urn:node:KNB"
#' @rdname D1Object-initialize
#' @seealso \code{\link[=D1Object-class]{D1Object}}{ class description.}
#' @aliases D1Object-initialize
setMethod("initialize", "D1Object", function(.Object, id, data, format, mnNodeId=as.character(NA)) {
  # Write the incoming data to disk and create the DataObject with this file
  
  if(format == "text/csv") {
    dataChar <- rawToChar(data)
    theData <- textConnection(dataChar)
    df <- read.csv(theData, stringsAsFactors=FALSE)
    tfile <- tempfile()
    write.csv(df, tfile)
  } else {
    dataChar <- rawToChar(data)
    tfile <- tempfile()
    writeLines(dataChar, tfile)
  }
  
  .Object@dataObject <- new("DataObject", id = id, format = format, mnNodeId = mnNodeId, filename=tfile)
  return(.Object)
})


#########################################################
### MNRead and MNStorage methods
#########################################################


#########################################################
### Utility methods
#########################################################

#' Get the data content of a D1Object.
#' @param x  D1Object the data structure from where to get the data
#' @rdname getData
#' @export
setMethod("getData", signature("D1Object"), function(x) {
  # We have to include all args for .Deprecated, because we are deprecating just this 
  # implementation (method) and not the generic. When .Deprecated is called from the method,
  # it doesn't properly identify the generic/method name (msg says ".local is deprecated)
  msg <- sprintf("'getData' is deprecated.\nUse 'datapack:getData' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("getData(x)")
  .Deprecated("getData", package="datapack", msg, methodSig)
  data <- rawToChar(getData(x@dataObject))
})

#' Get the Identifier of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @rdname getIdentifier
#' @return the identifier
#' @export
setMethod("getIdentifier", signature("D1Object"), function(x) {
  msg <- sprintf("'getIdentifier' is deprecated.\nUse 'datapack:getIdentifier' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("getIdentifier(x)")
  .Deprecated("getIdentifier", package="datapack", msg, methodSig)
  getIdentifier(x@dataObject)
})

#' Get the FormatId of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @return the formatId
#' @rdname getFormatId
#' @export
setMethod("getFormatId", signature("D1Object"), function(x) {
  msg <- sprintf("'getFormatId' is deprecated.\nUse 'datapack:getFormatId' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("getFormatId(x)")
  .Deprecated("getFormatId", package="datapack", msg, methodSig)
  getFormatId(x@dataObject)
})

#' Make the object publicly readable.
#' @description This method should be called prior to creating the object in DataONE.  
#' When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any effect on remotely uploaded copies of
#' the D1Object. 
#' @param x D1Object
#' @param ... (not yet used)
#' @return D1Object with modified access rules
#' @seealso \code{\link[=DataObject-class]{DataObject}}{ class description.}
#' @rdname setPublicAccess
#' @export
setMethod("setPublicAccess", signature("D1Object"), function(x) {
  msg <- sprintf("'setPublicAccess' is deprecated.\nUse 'datapack:setPublicAccess' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("setPublicAccess(x)")
  .Deprecated("setPublicAccess", package="datapack", msg, methodSig) 
  x@dataObject <- setPublicAccess(x@dataObject)
  return(x)
})

#' Test whether the provided subject can read an object.
#' @description Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission to a repository, 
#' and will show the permissions that would be enforced by the repository on submission.
#' Currently it only uses the AccessPolicy to determine who can read (and not the rightsHolder field,
#' which always can read an object).  If an object has been granted read access by the
#' special "public" subject, then all subjects have read access.
#' @details The subject name used in both the AccessPolicy and in the \code{'subject'}
#' argument to this method is a string value, but is generally formatted as an X.509
#' name formatted according to RFC 2253.
#' @param x D1Object
#' @param subject : the subject name of the person/system to check for read permissions
#' @param ... Additional arguments
#' @return logical TRUE if the subject has read permission, or FALSE otherwise
#' @rdname canRead
#' @export
setMethod("canRead", signature("D1Object"), function(x, subject) {
  msg <- sprintf("'canRead' is deprecated.\nUse 'datapack:canRead' instead.\nSee help(\"Deprecated\") and help(\"dataone-deprecated\").")
  methodSig <- sprintf("canRead(x)")
  .Deprecated("canRead", package="datapack", msg, methodSig) 
  canRead(x@dataObject, subject)
})

#' @title return the D1Object data as a data.frame.
#' @description This method uses the provided metadata reference object for instructions on
#' how to parse the data table (which parameters to set)
#' 'reference' is the metadata D1Object that gives instruction on how to read the data
#' into the dataFrame
#' @param x A D1Object
#' @param reference A reference to a D1Object
#' @param ... (Additional parameters)
#' @rdname asDataFrame
#' @aliases asDataFrame
#' @export
setGeneric("asDataFrame", function(x, reference, ...) { 
  .Deprecated("read.csv", "base")
  standardGeneric("asDataFrame")
})

#' @rdname asDataFrame
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

#' @rdname asDataFrame
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

#' @rdname asDataFrame
#' @export
setMethod("asDataFrame", signature("D1Object"), function(x, ...) {
    ## Load the data into a dataframe
    # Prevent .Deprecated warnings (in code called internally)
    suppressWarnings(dataBytes <- getData(x))
    theData <- textConnection(dataBytes)
    message("theData is ", class(theData))
    ## using read.csv instead of read.table, because it exposes the defaults we want
    ## while also allowing them to be overriden
    df <- read.csv(theData, ...)
    return(df)
})

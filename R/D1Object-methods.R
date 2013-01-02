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

### This file contains the methods and accessors for D1Object objects

#########################################################
### MNRead and MNStorage methods
#########################################################

## buildD1Object, implemented as a static way of constructing an object
# TODO: Consider making this into a real constructor
# Currently this is just a copy from the D1Client class, needs to be
# refactored.
#setGeneric("buildD1Object", function(id, data, format, mn_nodeid, ...) { 
#  standardGeneric("buildD1Object")
#})
#
#setMethod("buildD1Object",
#          signature("character", "character", "character", "character"),
#	  function(id, data, format, mn_nodeid) {
#
  # build identifier to be used in system metadata
#  pid <- .jnew("org/dataone/service/types/v1/Identifier")
#  pid$setValue(id)

  # Set up/convert additional system metadata fields
  # get the submitter from the certificate
#  certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
#  cert <- certman$loadCertificate()
#  submitter <- .jnew("org/dataone/service/types/v1/Subject")
#  submitter$setValue(certman$getSubjectDN(cert))

  # Convert incoming data to byte array (byte[])
#  ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
#  byteArray <- ioUtils$toByteArray(data)

  # build the ObjectFormatIdentifier.
#  format.id <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
#  format.id$setValue(format)

  # build the NodeReference from the mn_nodeid.
#  if(is.null(mn_nodeid) || (mn_nodeid == "")) {
#    print("ERROR: A Member Node must be defined to create an object.")
#    return(.jnull("org/dataone/client/D1Object"))
#  }
#  mn.noderef <- .jnew("org/dataone/service/types/v1/NodeReference")
#  mn.noderef$setValue(mn_nodeid)

  # Now build the object with the sysmeta values
#  d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, format.id,
#                    submitter, mn.noderef, check=FALSE)
  #message("building object")
#  if (!is.null(e <- .jgetEx())) {
#    print("Java exception was raised")
#    print(.jcheck(silent=TRUE))
#    print(.jcheck(silent=TRUE))
#    print(e)
#  }

#  return(d1object)
#})

#########################################################
### Accessor methods
#########################################################

# Need accessors for sysmeta and data contained in the 
# internal java D1Object instance

#########################################################
### Utility methods
#########################################################

setGeneric("getData", function(x, id, ...) {
    standardGeneric("getData")
})
 
setMethod("getData", signature("D1Object"), function(x) {
    message("@@r1")
	jD1Object = x@jD1o
	message("@@r2")
	if(!is.jnull(jD1Object)) {
		print ("@@r3")
		databytes <- jD1Object$getData()
		if(is.null(databytes)) {
			print(paste("Didn't find data in:", id))
			return()
		}
		jDataString <- .jnew("java/lang/String",databytes,"UTF-8")
		if (!is.null(e<-.jgetEx())) {
			print("Java exception was raised")
			print(e)
			print(.jcheck(silent=FALSE))
		}
		dataString <- jDataString$toString()
		return(dataString)
	}
})



setGeneric("getIdentifier", function(x, ...) {
    standardGeneric("getIdentifier")
})

setMethod("getIdentifier", signature("D1Object"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPid <- jD1Object$getIdentifier()
		if(is.null(jPid)) {
			return()
		}
		return(jPid$getValue())
	}
})


setGeneric("getFormatId", function(x, ...) {
			standardGeneric("getFormatId")
		})

setMethod("getFormatId", signature("D1Object"), function(x) {
			jD1Object = x@jD1o
			if(!is.jnull(jD1Object)) {
				jFormatId <- jD1Object$getFormatId()
				if(is.null(jFormatId)) {
					return()
				}
				return(jFormatId$getValue())
			}
		})


setGeneric("setPublicAccess", function(x, ...) {
  standardGeneric("setPublicAccess")
})


setMethod("setPublicAccess", signature("D1Object"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			message("setPublicAccess: got policy editor")
			jPolicyEditor$setPublicAccess()
		} else {
			print("policy editor is null")
		}
	}
})



setGeneric("canRead", function(x, subject, ...) {
  standardGeneric("canRead")
})

setMethod("canRead", signature("D1Object", "character"), function(x, subject) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			message("canRead: got policy editor")
			jSubject <- J("org/dataone/client/D1TypeBuilder", "buildSubject", subject)
			jPermission <- J("org/dataone/service/types/v1/Permission", "convert", "read")
			result <- jPolicyEditor$hasAccess(jSubject,jPermission)
		} else {
			print("policy editor is null")
			result <- FALSE
		}
	}
	return(result)
})


## the generic asDataFrame function returns the data as a DataFrame, assuming that
## the object represents data and can be coerced into tabular form.
## 'x' as per usual, represents the class of the implementing method
## 'reference' is either the identifier of the data, or the metadata D1Object, depending
## on the implementing method.

setGeneric("asDataFrame", function(x, reference, ...) { standardGeneric("asDataFrame")} )

##
## this method uses the provided metadata reference object for instructions on
## how to parse the data table (which parameters to set)
## 'reference' is the metadata D1Object that gives instruction on how to read the data
## into the dataFrame
##
setMethod("asDataFrame", signature("D1Object", "D1Object"), function(x, reference, ...) {
    ## reference is a metadata D1Object
	mdFormat <- getFormatId(reference)
	
	dtdClassName <- dataTableDescriber.registry[[ mdFormat ]]
	message(paste("@@ asDataFrame/Object", dtdClassName))
	if (!is.na(dtdClassName)) {
		dtd <-	do.call(dtdClassName, list(reference))
		df <- asDataFrame(x,dtd)
		## con <-textConnection("signatures", "w")
		## showMethods("documents.d1FormatIds", printTo=con)
		## close(con)
		## regMdParsers <- na.omit(sapply(strsplit(signatures[2:length(signatures)],"\""), 
		##             function(x) ifelse(length(x>1),x[2],NA)))
	} else {
		print("Could not find metadata parser, trying as plain csv...")
		df <-  asDataFrame(x)
	}
	return( df )
})
             
setMethod("asDataFrame", signature("D1Object", "DataTableDescriber"), function(x, reference, ...) {
                        
    message(paste("@@ asDataFrame / D1Object-dtd",class(reference)))
    ## reference is a DataTableDescriber
	pids <- documented.d1Identifiers(reference)
	jDataId <- x@jD1o$getIdentifier()$getValue()
	index <- which(pids == jDataId)
	message(paste("Index of data item is",index))

	## is this a datatype that we can handle?
	dataFormat <- dataTable.dataFormat(reference,index)
	if (dataFormat != "text/simpleDelimited") {
		print(paste("cannot process data of type", dataFormat))
		return()
	} else if (dataTable.attributeOrientation(reference, index) == 'row') {
		print(paste("cannot process text/simpleDelimited file where attributes are by row"))
	}
	
	fieldSeparator <- dataTable.fieldDelimiter(reference, index)
	if (is.na(fieldSeparator))
		fieldSeparator <- ","

	quoteChar <- dataTable.quoteCharacter(reference, index)
	if (is.na(quoteChar))
		quotChar <- "\""

	missingValues <- dataTable.missingValueCodes(reference,index)
	missingValues <- subset(missingValues, !is.na(missingValues))
	if(length(missingValues)==0)
		missingValues <- "NA"
	
	encoding <- dataTable.characterEncoding(reference, index)
	if (is.na(encoding))
		encoding <- "unknown"

	skip <- dataTable.skipLinesHeader(reference, index)
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
##  can any of the parameters from read.table to override default behavior 
##  (see read.csv and read.table)
##
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


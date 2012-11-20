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
  #print("building object")
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
    print("@@r1")
	jD1Object = x@jD1o
	print ("@@r2")
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


## setMethod("getData", signature("jobJRef"), function(x) {

##   if (.jinstanceof(x, "org/dataone/client/D1Object") {
##     jD1Object = x

##     if(!is.jnull(jD1Object)) {
##       print ("@@r3")
##       databytes <- jD1Object$getData()
##       print ("@@r4")
##       if(is.null(databytes)) {
##         print(paste("Didn't find data in:", id))
##         return()
##       }
##       return(databytes)
##     }
##   } else {
##     print("Passed in Java object not the right type (org.dataone.client.D1Object)")
##     return()
##   }
##})


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


setGeneric("setPublicAccess", function(x, ...) {
  standardGeneric("setPublicAccess")
})


setMethod("setPublicAccess", signature("D1Object"), function(x) {
    jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
		jPolicyEditor <- jD1Object$getAccessPolicyEditor()
		if (!is.jnull(jPolicyEditor)) {
			print("setPublicAccess: got policy editor")
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
			print("canRead: got policy editor")
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


## getData, returns data object at index
##setGeneric("asDataFrame", function(x, ...) { standardGeneric("asDataFrame")} )
setGeneric("asDataFrame", function(x, identifier, ...) { standardGeneric("asDataFrame")} )

setMethod("asDataFrame", signature("D1Object"), function(x) {
	## Load the data into a dataframe
	##df <- read.table(textConnection(x@dataList[[index]]), header = TRUE, sep = ",", na.strings = "-999")
	dataBytes <- getData(x)
    df <- read.csv(textConnection(dataBytes))
	return(df)
})


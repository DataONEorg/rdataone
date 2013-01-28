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


#' Get the Contents of the Specified Data Object
#' 
#' @param x  D1Object or DataPackage: the data structure from where to get the data
#' @param id Missing or character: if @code{x} is DataPackage, the identifier of the
#' package member to get data from
#' @param ... (not yet used)
#' @returnType character 
#' @return character representation of the data
#' 
#' @author rnahf
#' @export
setGeneric("getData", function(x, id, ...) {
    standardGeneric("getData")
})
 
setMethod("getData", signature("D1Object"), function(x) {
	jD1Object = x@jD1o
	if(!is.jnull(jD1Object)) {
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



#' Get the Identifier of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @returnType character
#' @return the identifier
#' 
#' @author rnahf
#' @export
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


#' Get the FormatId of the D1Object
#' @param x D1Object
#' @param ... (not yet used)
#' @returnType character
#' @return the formatId
#' 
#' @author rnahf
#' @export
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


#' Add a Rule to the AccessPolicy to Make the Object Publicly Readable
#' 
#' To be called prior to creating the object in DataONE.  When called before 
#' creating the object, adds a rule to the access policy that makes this object
#' publicly readable.  If called after creation, it will only change the system
#' metadata locally, and will not have any affect. 
#' @param x D1Object
#' @param ... (not yet used)
#' @returnType NULL
#' @return NULL
#' 
#' @author rnahf
#' @export
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



#' Test whether the provided subject can read the object
#' 
#' Using the AccessPolicy, tests whether the subject has read permission
#' for the object.  This method is meant work prior to submission, so uses
#' only the AccessPolicy to determine who can read (Not the rightsHolder field,
#' which always can read.)
#' @param x D1Client
#' @param subject : character
#' @param ... (not yet used)
#' @returnType logical
#' @return TRUE or FALSE
#' 
#' @author rnahf
#' @export
setGeneric("canRead", function(x, subject, ...) {
  standardGeneric("canRead")
})

## TODO: is this method really necessary? Can it be implemented more fully? (looking at rightsHolder)
## (want to be able to work prior to submission)
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





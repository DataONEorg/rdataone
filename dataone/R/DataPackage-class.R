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

setClass("DataPackage",
         representation(packageId = "character",
                        jDataPackage = "jobjRef")
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(...) { standardGeneric("DataPackage")} )
##setGeneric("DataPackage", function(identifier, jDataPackage, ...) {
##standardGeneric("DataPackage")
##})


setMethod("initialize", "DataPackage", function(.Object, packageId, jDataPackage) {
	message("@@ DataPackage-class.R initialize")
	## verify the jDataPackage
	if (missing("jDataPackage")) {
        message("@@ DataPackage-class.R - (missing jDataPackage)...")
		if (!missing("packageId")) {
            message("@@ DataPackage-class.R - (have packageId)...")
			## set the packageId and instantiate a new jDataPackage
			.Object@packageId <- packageId
			jPid <- .jnew("org/dataone/service/types/v1/Identifier")
			jPid$setValue(packageId)
			jDataPackage <- .jnew("org/dataone/client/DataPackage",jPid, check=FALSE)
			if (!is.null(e <- .jgetEx())) {
				print("Java exception was raised")
				print(.jcheck(silent=TRUE))
				print(.jcheck(silent=TRUE))
				print(e)
				jDataPackage <- .jnull("org/dataone/client/DataPackage")
			}
			.Object@jDataPackage <- jDataPackage
		}
	} else {
        message("@@ DataPackage-class.R - (have a jDataPackage...)")
		## check that jDataPackage is indeed one
		## then set the packageId from that one
		if (.jinstanceof(jDataPackage,"org/dataone/client/DataPackage")) {
            message("@@ DataPackage-class.R - (jDataPackage is a DataPackage instance)...")
			jPid <- jDataPackage$getPackageId()
			.Object@packageId <- jPid$getValue()
			.Object@jDataPackage <- jDataPackage
		}
	}
   return(.Object)
})


## One arg: pid
#setMethod("DataPackage",
#  function(identifier, ...) {
#
#  message(      "@@ DataPackage-class.R 07: Running in DataPackage constructor...")
#  message(paste("@@ DataPackage-class.R 08: Identifier is: ", identifier))
#
#  ## create new DataPackage object and insert identifier and data
###  res <- new("DataPackage")
#  message("@@ DataPackage-class.R 09:")
#  jPid <- .jnew("org/dataone/service/types/v1/Identifier")
#  jPid$setValue(identifier)
#  jDP <- .jnew("org/dataone/client/DataPackage",jPid)
#
#  res <- new(Class="DataPackage",identifier=identifier, jDataPackage=jDP)
#  message("@@ DataPackage-class.R 10:")
###  res@identifier <- identifier
#  
#  return(res)
#})
#
### Two args: pid and jDataPackage
#setMethod("DataPackage", signature(...), function(identifier, jDataPackage) {
# 
### create new DataPackage object and insert identifier and data
###  res <- 
#  message("DataPackage-class.R 07a: in 2 arg constructor")
#  # Verify first object.
#  if (exists("jDataPackage")) {
#	  message(jDataPackage)
#	  if (is.null(jDataPackage)) {
#		  print("found the problem")
#		  return(new(Class="DataPackage"))
#	  } else {
#		  print ("exists but not null")
#		  return(new(Class="DataPackage"))
#	  }
#  }
#  if(is.jnull(jDataPackage)) {
#    print("Cannot create DataPackage from null object")
#    return(new("DataPackage"))
#  } else if(!.jinstanceof(jDataPackage, J("org.dataone.client.DataPackage"))) {
#    print("First argument must be a Java DataPackage object.")
#    return(NULL)
#  }
#  message("@@ DataPackage-class.R 09a:")
#  res <- new(Class="DataPackage", identifier=identifier, jDataPackage=jDataPackage)
###  res@jDataPackage <- jDataPackage
#
#
###  res@jDataPackage <- jDataPackage
#  message("@@ DataPackage-class.R 10a:")
###  res@identifier <- identifier
#    
#  return(res)
#})


### Create an R DataPackage from a Java DataPackage.
#
### generic
#setGeneric("createDataPackage", function(jDataPackage, ...) {
#  standardGeneric("createDataPackage")
#})
#
#setMethod("createDataPackage", signature("jobjRef"),
#    function(jDataPackage) {
#  ## create new DataPackage object and insert identifier and data
#  res <- new("DataPackage")
#
#  # Verify first object.
#  if(is.jnull(jDataPackage)) {
#    print("Cannot create DataPackage from null object")
#    return(res)
#  } else if(!.jinstanceof(jDataPackage, J("org.dataone.client.DataPackage"))) {
#    print("First argument must be a Java DataPackage object.")
#    return(NULL)
#  }
#
#  # Save the Java objects (sysmeta may be null).
#  res@jDataPackage <- jDataPackage
#
#  # Get the pid.
#  pid <- jDataPackage$getPackageId()
#  res@identifier <- pid$getValue()
#  
#  
###  res@metaList <- ""
###  res@dataList <- ""
#
#  # Loop through all the identifiers, find the scimeta and scidata.
#  jResSet <- jDataPackage$identifiers()
#  num <- jResSet$size()
#  if(num > 0) {
#    resIterator <- .jrcall(jResSet, "iterator")
#    while(.jrcall(resIterator, "hasNext")) {
#      jResId <- .jrcall(resIterator, "next")
#      jD1Object <- jDataPackage$get(jResId)
#      jObjSysMeta <- jD1Object$getSystemMetadata()
#      if(!is.jnull(jObjSysMeta)) {
#	jFormatId <- jObjSysMeta$getFormatId()
#	if(!is.jnull(jFormatId)) {
#	  str <- substring(jFormatId$getValue(), 1, 4)
#	  # Should really go to the CN to determine if this is scimeta
#	  if(str == "eml:") {
#	    if(res@metaList != "") {
#	      message(paste("Found second scimeta in", jResId$getValue()))
#	    } else {
#	      databytes <- jD1Object$getData()
#	      asString <- .jnew("java/lang/String", databytes)
#	      res@metaList <- asString$toString()
#	      next
#	    }
#	  }
#	}
#      }
#
#      # Will only get here if the object isn't the first scimeta.
#      pid <- jResId$getValue()
#      res@dataList[[ pid ]] <- jD1Object
#    }
#  }
#
#  return(res)
#})

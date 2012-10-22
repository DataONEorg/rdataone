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
         representation(identifier = "character",
	                jDataPackage = "jobjRef",
                        sysmeta = "jobjRef",
                        scimeta = "character",
			dataList = "list")
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(identifier, sysmeta, scimeta, ...) {
  standardGeneric("DataPackage")
})

## One arg: pid
setMethod("DataPackage", signature("character"),
  function(identifier, ...) {

  null_sysmeta = .jnull("org/dataone/service/types/v1/SystemMetadata")

  res <- DataPackage(identifier, null_sysmeta, "")
  print("@@15:")
  return(res)
})

## Two args: pid, sysmeta
setMethod("DataPackage", signature("character", "jobjRef"),
  function(identifier, sysmeta, ...) {

  if(!.jinstanceof(sysmeta, J("org.dataone.service.types.v1.SystemMetadata"))) {
    print("Second argument must be a Java SystemMetadata object.")
    return(NULL)
  }

  res <- DataPackage(identifier, sysmeta, "")
  print("@@14:")
  return(res)
})

## All three args: pid, sysmeta, scimeta
setMethod("DataPackage", signature("character", "jobjRef", "character"),
  function(identifier, sysmeta, scimeta) {

  print("@@ 7: Running in DataPackage 3 arg constructor...")
  print(paste("@@ 8: Identifier is: ", identifier))
  ## create new DataPackage object and insert identifier and data
  res <- new("DataPackage")
  print("@@ 9:")
  res@jDataPackage <- .jnull("org/dataone/client/DataPackage")

  print("@@10:")
  res@identifier <- identifier
  print("@@11:")
  res@sysmeta <- sysmeta
  print("@@12:")
  if(!is.null(scimeta) && (scimeta != "")) {
    res@scimeta <- scimeta
  }
  print("@@13:")

  return(res)
})


## Create an R DataPackage from a Java DataPackage.

## generic
setGeneric("createDataPackage", function(jDataPackage, sysmeta, ...) {
  standardGeneric("createDataPackage")
})

setMethod("createDataPackage", signature("jobjRef", "jobjRef"),
    function(jDataPackage, sysmeta) {
  ## create new DataPackage object and insert identifier and data
  res <- new("DataPackage")

  # Verify first object.
  if(is.jnull(jDataPackage)) {
    print("Cannot create DataPackage from null object")
    return(res)
  } else if(!.jinstanceof(jDataPackage, J("org.dataone.client.DataPackage"))) {
    print("Second argument must be a Java DataPackage object.")
    return(NULL)
  }

  # Verify second object.
  if(!is.jnull(jDataPackage) && !.jinstanceof(sysmeta,
      J("org.dataone.service.types.v1.SystemMetadata"))) {
    print("Second argument must be null or a Java SystemMetadata object.")
    return(NULL)
  }

  # Save the Java objects (sysmeta may be null).
  res@jDataPackage <- jDataPackage
  res@sysmeta <- sysmeta

  # Get the pid.
  pid <- jDataPackage$getPackageId()
  res@identifier <- pid$getValue()
  
  # Make sure scimeta is blank at first in case there are 2 of them.
  res@scimeta <- ""
  res@dataList <- list()

  # Loop through all the identifiers, find the scimeta and scidata.
  jResSet <- jDataPackage$identifiers()
  num <- jResSet$size()
  if(num > 0) {
    resIterator <- .jrcall(jResSet, "iterator")
    while(.jrcall(resIterator, "hasNext")) {
      jResId <- .jrcall(resIterator, "next")
      jD1Object <- jDataPackage$get(jResId)
      jObjSysMeta <- jD1Object$getSystemMetadata()
      if(!is.jnull(jObjSysMeta)) {
	jFormatId <- jObjSysMeta$getFormatId()
	if(!is.jnull(jFormatId)) {
	  str <- substring(jFormatId$getValue(), 1, 4)
	  # Should really go to the CN to determine if this is scimeta
	  if(str == "eml:") {
	    if(res@scimeta != "") {
	      print(paste("Found second scimeta in", jResId$getValue()))
	    } else {
	      databytes <- jD1Object$getData()
	      asString <- .jnew("java/lang/String", databytes)
	      res@scimeta <- asString$toString()
	      next
	    }
	  }
	}
      }

      # Will only get here if the object isn't the first scimeta.
      pid <- jResId$getValue()
      res@dataList[[ pid ]] <- jD1Object
    }
  }

  return(res)
})

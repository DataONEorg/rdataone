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
                metaList = "list",
                dataList = "list")
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(identifier, metaList, dataList, ...) {
  standardGeneric("DataPackage")
})

## One arg: pid
setMethod("DataPackage", signature("character"),
  function(identifier, ...) {


  res <- DataPackage(identifier, "", "")
  print("@@ DataPackage-class.R 15:")
  return(res)
})


## All three args: pid, sysmeta, scimeta
setMethod("DataPackage", signature("character", "list", "list"),
  function(identifier, metaList, dataList) {

  print(      "@@ DataPackage-class.R 07: Running in DataPackage 3 arg constructor...")
  print(paste("@@ DataPackage-class.R 08: Identifier is: ", identifier))

  ## create new DataPackage object and insert identifier and data
  res <- new("DataPackage")
  print("@@ DataPackage-class.R 09:")
  res@jDataPackage <- .jnull("org/dataone/client/DataPackage")

  print("@@ DataPackage-class.R 10:")
  res@identifier <- identifier
  print("@@ DataPackage-class.R 11:")
  res@metaList <- metaList
  print("@@ DataPackage-class.R 12:")
  res@dataList <- dataList
  
  return(res)
})


## Create an R DataPackage from a Java DataPackage.

## generic
setGeneric("createDataPackage", function(jDataPackage, ...) {
  standardGeneric("createDataPackage")
})

setMethod("createDataPackage", signature("jobjRef"),
    function(jDataPackage) {
  ## create new DataPackage object and insert identifier and data
  res <- new("DataPackage")

  # Verify first object.
  if(is.jnull(jDataPackage)) {
    print("Cannot create DataPackage from null object")
    return(res)
  } else if(!.jinstanceof(jDataPackage, J("org.dataone.client.DataPackage"))) {
    print("First argument must be a Java DataPackage object.")
    return(NULL)
  }

  # Save the Java objects (sysmeta may be null).
  res@jDataPackage <- jDataPackage

  # Get the pid.
  pid <- jDataPackage$getPackageId()
  res@identifier <- pid$getValue()
  
  
##  res@metaList <- ""
##  res@dataList <- ""

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

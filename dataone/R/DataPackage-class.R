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


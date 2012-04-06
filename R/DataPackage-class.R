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
                        sysmeta = "jobjRef",
                        scimeta = "character",
			dataList = "list")
)

###########################
## DataPackage constructors
###########################

## generic
setGeneric("DataPackage", function(identifier, sysmeta1, scimeta1, ...) { standardGeneric("DataPackage")} )

## first arg is a character
setMethod("DataPackage", "character",
    function(identifier, sysmeta1, scimeta1) {

    ## create new DataPackage object and insert identifier and data
    res <- new("DataPackage")
    res@identifier <- identifier
    res@sysmeta <- sysmeta1
    res@scimeta <- scimeta1

    return(res)
})

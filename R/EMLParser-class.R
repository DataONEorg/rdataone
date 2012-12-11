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


##############################
## EMLParser class definition - note that it inherits from DataTableDescriber
##############################
setClass("EMLParser", 
        representation(d1Object = "D1Object", xmlDocRoot = "XMLNode"),
        contains="DataTableDescriber",
        prototype=prototype(new("DataTableDescriber")))


###########################################################
### define here which DataONE FormatIds this class handles
###########################################################
if (!exists("dataTableDescriber.registry")) dataTableDescriber.registry <- list()
dataTableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.1" ]] <- "EMLParser" 
dataTableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.0" ]] <- "EMLParser" 
dataTableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.1" ]] <- "EMLParser" 
dataTableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.0" ]] <- "EMLParser" 



##########################
## EMLParser constructors
##########################

## generic
setGeneric("EMLParser", function(d1Object, ...) { standardGeneric("EMLParser")} )

setMethod("EMLParser", signature("D1Object"), function(d1Object) {
            
    xmlDocRoot <- xmlRoot(xmlTreeParse(as.character(getData(d1Object))))  
    result <- new("EMLParser", d1Object=d1Object, xmlDocRoot=xmlDocRoot)
    return(result)
})

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


##############################
## EMLParser class definition - note that it inherits from TableDescriber
##############################


## Handler for Parsing Table Format Details from Metadata
## 
## @description
## Implements methods to provide parsing instructions for asDataFrame.  
## @details 
## handles eml formats 2.0.0 through 2.1.1
## 
## @slot d1Object the metadata object 
## @slot xmlDocRoot the xml representation of the metadata
## @author rnahf
## @import XML
## @exportClass EMLParser
setClass("EMLParser", 
        representation(d1Object = "D1Object", xmlDocRoot = "XMLNode"),
        contains="AbstractTableDescriber")
##        contains="AbstractTableDescriber",
##        prototype=prototype(new("AbstractTableDescriber")))


###########################################################
### define here which DataONE FormatIds this class handles
###########################################################
if (!exists("tableDescriber.registry")) tableDescriber.registry <- list()
tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.1" ]] <- "EMLParser" 
tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.1.0" ]] <- "EMLParser" 
tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.1" ]] <- "EMLParser" 
tableDescriber.registry[[ "eml://ecoinformatics.org/eml-2.0.0" ]] <- "EMLParser" 



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

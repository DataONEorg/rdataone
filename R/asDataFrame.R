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
#
# Author: rnahf
###############################################################################



## TODO: turn these back into roxygen comments when roxygen3 becomes available
## (roxygen2 doesn't handle overloaded methods well)  (roxygen comments start
## with pound-singlequote)
##
## Build a DataFrame From a Data Object
## 
## The generic asDataFrame function returns the content of a DataONE object as 
## a DataFrame, assuming that the object represents scientific data and can be 
## coerced into tabular form.  Several methods exist to accomplish this.
## 
## 
## @details 
## In many cases, a science metadata object includes in its description how the 
## scientific data is stored, and this information should be used when converting
## from tabular form to a dataFrame.  The 2-argument implementations for asDataFrame
## exist to allow this to happen.  
## 
## Most users will use the implementation that takes the DataPackage and the member
## identifier of the data object to read into a dataFrame.  In this case, the 
## the dataPackage isolates the appropriate science metadata object, finds the 
## appropriate TableDescriber for it, and passes it to the single-argument
## \code{asDataFrame(D1Object,...)} method.
## 
## The other double-argument methods are progressively more direct, and are most
## useful when there is not a DataPackage that can associate the metadata to the
## data.  
## 
## The TableDescriber class is abstract, with data-format-specific subclasses
## providing the specific logic for parsing that particular data format (for example
## EMLParser for reading and parsing EML documents).  If there is no metadata parser
## for the metadata that describes the object, you will have to determine the 
## correct parameters for read.csv manually.
##  
## @note
## As of Feb 2013, there is only a metadata parser for EML documents.  Other parsers
## should become available as separate packages that can be loaded separately. 
## 
## 
## 
## @param x : of type DataPackage or D1Object. Where the object is.
## @param reference : either the identifier for the data, the D1Object of the metadata
## that describes it, the TableDescriber object, or missing
## @param ... : overriding parameters for read.csv
## 
## @returnType dataFrame
## @return a dataFrame representation of the object's data
## 
## @author rnahf
## @export
## 
## @examples 
##   \dontrun{
## df <- asDataFrame(data.package, dataMember.id)
## 
## df <- asDataFrame(data.object, its.metadata)
## 
## table.describer <- EMLParser(its.metadata)
## df <- asDataFrame(data.object, table.describer)
## 
## df <- asDataFrame(data.object, sep="\t", ...)
## 
## df <- asDataFrame(data.object)
## 
##   }
## 
## @seealso \code{\link{read.csv}} 
## 
## @docType methods
## @rdname asDataFrame-methods


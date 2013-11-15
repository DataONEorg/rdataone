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

## A class representing a Member Node repository, which can expose and store data
## @slot nodeid The node identifier of the MN
## @slot endpoint The baseurl of the MN
## @author jones
## @export
setClass("MNode",
         representation(nodeid = "character",
						endpoint = "character")
)

#########################
## MNode constructors
#########################

## @param nodeid The node identifier with which this node is registered in DataONE
## @param ... (not yet used)
## @returnType MNode  
## @return the MNode object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("MNode", function(nodeid, ...) {
  standardGeneric("MNode")
})

## 
## Construct a MNode, using default ("KNB")
## @name MNode
## 
## @returnType MNode  
## @return the MNode object representing repository in DataONE
## 
## @author jones
## @docType methods
## @export
setMethod("MNode", , function() {
    result <- MNode("urn:node:KNB")
    return(result)
})

## Construct a MNode, using a passed in node identifier
## @param nodeid The node identifier with which this node is registered in DataONE
## @returnType MNode  
## @return the MNode object representing the DataONE environment
## 
## @author jones
## @export
setMethod("MNode", signature("character"), function(nodeid) {

  ## create new MNode object and insert uri endpoint
  result <- new("MNode")
  result@nodeid <- nodeid
  result@endpoint <- CN_URI

  return(result)
})

##########################
## Methods
##########################

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.ping
# public Date ping() 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.getLogRecords
# public Log getLogRecords(Date fromDate, Date toDate, Event event, String pidFilter, Integer start, Integer count) 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.getCapabilities
# public Node getCapabilities() 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.listObjects
# public InputStream get(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.getSystemMetadata
# public SystemMetadata getSystemMetadata(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.describe
# public DescribeResponse describe(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.getChecksum
# public Checksum getChecksum(Identifier pid, String checksumAlgorithm)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.listObjects
# public ObjectList listObjects(Date fromDate, Date toDate, ObjectFormatIdentifier formatid, Boolean replicaStatus, Integer start, Integer count) 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_storage.create
# public Identifier create(Identifier pid, InputStream object, SystemMetadata sysmeta) 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_storage.update
# public Identifier update(Identifier pid, InputStream object, Identifier newPid, SystemMetadata sysmeta) 
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_storage.delete
# public Identifier delete(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_storage.archive
# public Identifier archive(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.generateIdentifier
# public Identifier generateIdentifier(String scheme, String fragment)


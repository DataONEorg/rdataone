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
## @slot identifier The node identifier of the MN
## @slot name The node name
## @slot description The node description
## @slot subject The Distinguished Name of this node, used for authentication
## @slot contactSubject The Distinguished Name of contact person for this node
## @slot baseURL The registered baseURL for the node, which does not include the version string
## @slot endpoint The url to access node services, which is the baseURL plus the version string
## @author jones
## @export
setClass("MNode",
         representation(identifier = "character",
						name = "character",
						description = "character",
						baseURL = "character",
						subject = "character",
						contactSubject = "character",
						endpoint = "character")
)

#########################
## MNode constructors
#########################

## @param baseurl The node URL with which this node is registered in DataONE
## @param ... (not yet used)
## @returnType MNode  
## @return the MNode object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("MNode", function(baseurl, ...) {
  standardGeneric("MNode")
})

## Construct a MNode, using a passed in node url
## @param baseurl The node url with which this node is registered in DataONE
## @returnType MNode  
## @return the MNode object representing the DataONE environment
## 
## @author jones
## @export
setMethod("MNode", signature("character"), function(baseurl) {

	## create new MNode object and insert uri endpoint
	mnode <- new("MNode")
	mnode@endpoint <- baseurl

	## Lookup the rest of the node information
	xml <- getCapabilities(mnode)
	mnode@identifier <- xmlValue(xmlRoot(xml)[["identifier"]])
	mnode@name <- xmlValue(xmlRoot(xml)[["name"]])
	mnode@description <- xmlValue(xmlRoot(xml)[["description"]])
	mnode@baseURL <- xmlValue(xmlRoot(xml)[["baseURL"]])
	mnode@subject <- xmlValue(xmlRoot(xml)[["subject"]])
	mnode@contactSubject <- xmlValue(xmlRoot(xml)[["contactSubject"]])
	return(mnode)
})

##########################
## Methods
##########################

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.ping
# public Date ping() 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.getLogRecords
# public Log getLogRecords(Date fromDate, Date toDate, Event event, String pidFilter, Integer start, Integer count) 

## Get the node capabilities description, and store the information in the MNode
## @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.getCapabilities
## @param identifier The node identifier with which this node is registered in DataONE
## @returnType MNode  
## @return the MNode object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("getCapabilities", function(mnode, ...) {
    standardGeneric("getCapabilities")
})

setMethod("getCapabilities", signature("MNode"), function(mnode) {
	url <- paste(mnode@endpoint, "node", sep="/")
	response <- GET(url)
    if(response$status != "200") {
		return(null)
	}
	xml <- content(response)
	return(xml)
})

## Get the data associated with an object on this Member Node
## @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.get
## @param mnode The MNode instance from which the pid will be downloaded
## @param pid The object identifier to be downloaded
## @returnType data  
## @return the data object or a parsed representation of it
## 
## @author jones
## @export
setGeneric("get", function(mnode, pid, ...) {
    standardGeneric("get")
})

setMethod("get", signature("MNode", "character"), function(mnode, pid) {
	# TODO: add authentication to call if a certificate is available
	# TODO: need to properly URL-escape the PID
	url <- paste(mnode@endpoint, "object", pid, sep="/")
	response <- GET(url)
    if(response$status != "200") {
		return(null)
	}
	return(content(response))
})

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

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNQuery.query
# public InputStream query(String queryEngine, String query)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNQuery.getQueryEngineDescription
# public QueryEngineDescription getQueryEngineDescription(String queryEngine)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNQuery.listQueryEngines
# public QueryEngineList listQueryEngines()

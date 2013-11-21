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
## @slot baseURL The registered baseURL for the node, which does not include the version string
## @slot subject The Distinguished Name of this node, used for authentication
## @slot contactSubject The Distinguished Name of contact person for this node
## @slot replicate  a boolean flag indicating whether the node accepts replicas
## @slot type the node type, either 'mn' or 'cn'
## @slot state an indication of whether the node is accessible, either 'up' or 'down'
## @author jones
## @export
setClass("Node",
         slots = c(	identifier = "character",
					name = "character",
					description = "character",
					baseURL = "character",
					#services,
					#synchronization,
					#nodeReplicationPolicy,
					#ping,
					subject = "character",
					contactSubject = "character",
					replicate = "character",
					type = "character",
					state = "character"
					)
)

#########################
## Node constructors
#########################

## @param baseurl The node URL with which this node is registered in DataONE
## @param ... (not yet used)
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("Node", function(xml, ...) {
  standardGeneric("Node")
})

## Construct a Node, using a passed in node url
## @param baseurl The node url with which this node is registered in DataONE
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setMethod("Node", signature("XMLInternalElementNode"), function(xml) {

	## create new Node object
	node <- new("Node")
	parseCapabilities(node, xml)
	return(node)
})

##########################
## Methods
##########################

## @param baseurl The node URL with which this node is registered in DataONE
## @param ... (not yet used)
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("parseCapabilities", function(node, xml, ...) {
  standardGeneric("parseCapabilities")
})

## Construct a Node, using a passed in node url
## @param baseurl The node url with which this node is registered in DataONE
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setMethod("parseCapabilities", signature("Node", "XMLInternalElementNode"), function(node, xml) {
  
  ## Parse the rest of the node information
  node@identifier <- xmlValue(xml[["identifier"]])
  node@name <- xmlValue(xml[["name"]])
  node@description <- xmlValue(xml[["description"]])
  node@baseURL <- xmlValue(xml[["baseURL"]])
  node@subject <- xmlValue(xml[["subject"]])
  node@contactSubject <- xmlValue(xml[["contactSubject"]])
  attrs <- xmlAttrs(xml)
  node@replicate <- attrs[["replicate"]]
  node@type <- attrs[["type"]]
  node@state <- attrs[["state"]]
  return(node)
})

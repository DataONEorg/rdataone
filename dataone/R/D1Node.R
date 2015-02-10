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
setClass("D1Node",
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
setGeneric("D1Node", function(xml, ...) {
  standardGeneric("D1Node")
})

## Construct a Node, using a passed in node url
## @param baseurl The node url with which this node is registered in DataONE
## @returnType Node  
## @return the Node object representing the DataONE environment
## 
## @author jones
## @export
setMethod("D1Node", signature("XMLInternalElementNode"), function(xml) {

  ## create new Node object
	node <- new("D1Node")
	newnode <- parseCapabilities(node, xml)
	return(newnode)
})

##########################
## Methods
##########################

# The MN and CN APIs have several services with the same name, i.e. "get', 'getSystemMetadata', 'describe', etc.,
# so MNode.R and CNode.R have several methods that also share the same name. The generic functions for these 
# methods are defined here in the parent class, so that the generic is defined for all child classes (MNode.R, CNode.R), where
# the corresponding methods are defined.

#' Get the bytes associated with an object on this Node.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The Node instance from which the pid will be downloaded
#' @param pid The identifier of the object to be downloaded
#' @return the bytes of the object
#' @export
setGeneric("get", function(node, pid, ...) {
  standardGeneric("get")
})

#' Get the metadata describing system properties associated with an object on this Node.
#' @description The SystemMetadata includes information about the identity, type, access control, and other system
#' level details about the object.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The Node instance from which the SystemMetadata will be downloaded
#' @param pid The identifier of the object
#' @return SystemMetadata for the object
#' @import datapackage
#' @export
setGeneric("getSystemMetadata", function(node, pid, ...) {
  standardGeneric("getSystemMetadata")
})

#' This method provides a lighter weight mechanism than getSystemMetadata() for a client to
#' determine basic properties of the referenced object.
#' @param mnode The Node instance from which the identifier will be generated
#' @param pid Identifier for the object in question. May be either a PID or a SID. Transmitted as
#' part of the URL path and must be escaped accordingly.
#' @return A list of header elements
#' @export
#' @author Scott Chamberlain
setGeneric("describe", function(node, pid, ...) {
  standardGeneric("describe")
})

## Construct a Node, using a passed in capabilities XML
## @param node The node to which capabilities should be applied.
## @param ... (not yet used)
## @returnType Node  
## @return the Node object with modified capabilities properties from the XML
## 
## @author jones
## @export
setGeneric("parseCapabilities", function(node, xml, ...) {
  standardGeneric("parseCapabilities")
})

## Construct a Node, using a passed in capabilities XML
## @param node The node to which capabilities should be applied.
## @param xml The XML capabilities representing the node to be created
## @returnType Node  
## @return the Node object with modified capabilities properties from the XML
## 
## @author jones
## @export
setMethod("parseCapabilities", signature("D1Node", "XMLInternalElementNode"), function(node, xml) {
  
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

## This function parses a DataONE service response message for errors, and extracts and
## prints error information.
## @param x The DataONE service response
##
## @author Scott Chamberlain
d1_errors <- function(x){
  headnames <- names(x$headers)
  tmp <- grep('dataone-exception-description', headnames, value = TRUE)
  exc_name <- x$headers$`dataone-exception-name`
  detailcode <- x$headers$`dataone-exception-detailcode`
  mssg <- sub('dataone-exception-description: ', '', tmp)
  cat(sprintf('Exception name: %s', exc_name), "\n")
  cat(sprintf('Exception detail code: %s', detailcode), "\n")
  cat(sprintf('Exception description: %s', mssg), "\n")
  #  list(exc_name=exc_name, detailcode=detailcode, message=mssg)
}

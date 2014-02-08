#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2014
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

## A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
## and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
## access policy, replication policy, and related metadata.
## @slot serialVersion 
## @slot identifier              = "character",
## @slot formatId                = "character",
## @slot size                    = "numeric",
## @slot checksum                = "character",
## @slot checksumAlgorithm       = "character",
## @slot submitter               = "character",
## @slot rightsHolder            = "character",
## @slot accessPolicy            = "character",
## @slot replicationPolicy       = "character",
## @slot obsoletes               = "character",
## @slot obsoletedBy             = "character",
## @slot archived                = "logical",
## @slot dateUploaded            = "date",
## @slot dateSysMetadataModified = "date",
## @slot originMemberNode        = "character",
## @slot authoritativeMemberNode = "character",
## @slot replica                 = "character",
##
## @author jones
## @export
setClass("SystemMetadata", slots = c(
    serialVersion           = "numeric",
    identifier              = "character",
    formatId                = "character",
    size                    = "numeric",
    checksum                = "character",
    checksumAlgorithm       = "character",
    submitter               = "character",
    rightsHolder            = "character",
    #accessPolicy            = "AccessPolicy",
    #replicationPolicy       = "ReplicationPolicy",
    obsoletes               = "character",
    obsoletedBy             = "character",
    archived                = "logical",
    dateUploaded            = "character",
    dateSysMetadataModified = "character",
    originMemberNode        = "character",
    authoritativeMemberNode = "character"
    #replica                 = "character",
    ), )

#########################
## MNode constructors
#########################

## Construct a SystemMetadata, with all fields as null
## @returnType SystemMetadata  
## @return the SystemMetadata object representing an object
## 
## @author jones
## @export
setGeneric("SystemMetadata", function(x) {
  standardGeneric("SystemMetadata")
})

## Construct a SystemMetadata, with all fields as null
## @returnType SystemMetadata  
## @return the SystemMetadata object representing an object
## 
## @author jones
## @export
setMethod("SystemMetadata", signature(), function(x) {

	## create new SystemMetadata object
	sysmeta <- new("SystemMetadata")
    sysmeta@serialVersion <- 1
	return(sysmeta)
})

## TODO: Constructor that  takes XML as input

##########################
## Methods
##########################

# TODO: method to parse SystemMetadata from XML

## Parse an XML representation of system metadata, and set the object slots with obtained values.
## @param xml the XML representation of the capabilities, as an XMLInternalElementNode
## @returnType SystemMetadata  
## @return the SystemMetadata object representing an object
## 
## @author jones
## @export
parseSystemMetadata <- function(sysmeta, xml) {

    sysmeta@serialVersion <- xmlValue(xml[["serialVersion"]])
    sysmeta@identifier <- xmlValue(xml[["identifier"]])
    sysmeta@formatId <- xmlValue(xml[["formatId"]])
    sysmeta@size <- xmlValue(xml[["size"]])
    sysmeta@checksum <- xmlValue(xml[["checksum"]])
    #TODO: sysmeta@checksumAlgorithm
    sysmeta@submitter <- xmlValue(xml[["submitter"]])
    sysmeta@rightsHolder <- xmlValue(xml[["rightsHolder"]])
    #TODO: sysmeta@accessPolicy  
    #TODO: sysmeta@replicationPolicy
    sysmeta@obsoletes <- xmlValue(xml[["obsoletes"]])
    sysmeta@obsoletedBy <- xmlValue(xml[["obsoletedBy"]])
    sysmeta@archived <- xmlValue(xml[["archived"]])
    sysmeta@dateUploaded <- xmlValue(xml[["dateUploaded"]])
    sysmeta@dateSysMetadataModified <- xmlValue(xml[["dateSysMetadataModified"]])
    sysmeta@originMemberNode <- xmlValue(xml[["originMemberNode"]])
    sysmeta@authoritativeMemberNode <- xmlValue(xml[["authoritativeMemberNode"]])
    #TODO: sysmeta@replica    
    
    #attrs <- xmlAttrs(xml)
    #sysmeta@replicate <- attrs[["replicate"]]
    #sysmeta@type <- attrs[["type"]]
    #sysmeta@state <- attrs[["state"]]

    return(sysmeta)
}

# TODO: method to serialize XML from object


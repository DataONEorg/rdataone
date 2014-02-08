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

# TODO: method to serialize XML from object


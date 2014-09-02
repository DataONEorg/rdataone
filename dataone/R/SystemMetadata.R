
setClass("SystemMetadata", slots = c(
    serialVersion           = "numeric",
    identifier              = "character",
    formatId                = "character",
    size                    = "numeric",
    checksum                = "character",
    checksumAlgorithm       = "character",
    submitter               = "character",
    rightsHolder            = "character",
    accessPolicy            = "data.frame",  # accessPolicy (allow+ (subject+, permission+))
    replicationAllowed       = "logical",
    numberReplicas          = "numeric",
    preferredNodes          = "list",
    blockedNodes            = "list",
    obsoletes               = "character",
    obsoletedBy             = "character",
    archived                = "logical",
    dateUploaded            = "character",
    dateSysMetadataModified = "character",
    originMemberNode        = "character",
    authoritativeMemberNode = "character"
    #replica                 = "character",
    ), )


#'@description A class representing DataONE SystemMetadata, which is core information about objects stored in a repository
#' and needed to manage those objects across systems.  SystemMetadata contains basic identification, ownership,
#' access policy, replication policy, and related metadata.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{serialVersion}:}{value of type \code{"numeric"}, the current version of this system metadata; only update the current version}
#'    \item{\code{identifier}:}{value of type \code{"character"}, the identifier of the object that this system metadata describes.}
#'    \item{\code{replicationAllowed}:}{value of type \code{"logical"}, for replication policy allows replicants.}
#'    \item{\code{numberReplicas}:}{value of type \code{"numeric"}, for number of supported replicas.}
#'    \item{\code{preferredNodes}:}{value of type \code{"list"}, of prefered member nodes.}
#'    \item{\code{blockedNodes}:}{value of type \code{"list"}, of blocked member nodes.}
#'    \item{\code{formatId}:}{value of type \code{"character"}, the DataONE object format for the object.}
#'    \item{\code{size}:}{value of type \code{"numeric"}, the size of the object in bytes.}
#'    \item{\code{checksum}:}{value of type \code{"character"}, the checksum for the object using the designated checksum algorithm.}
#'    \item{\code{checksumAlgorithm}:}{value of type \code{"character"}, the name of the hash function used to generate a checksum, from the DataONE controlled list.}
#'    \item{\code{submitter}:}{value of type \code{"character"}, the Distinguished Name or identifier of the person submitting the object.}
#'    \item{\code{rightsHolder}:}{value of type \code{"character"}, the Distinguished Name or identifier of the person who holds access rights to the object.}
#'    \item{\code{accessPolicy}:}{value of type \code{"data.frame"}, a list of access rules to be applied to the object.}
#'    \item{\code{obsoletes}:}{value of type \code{"character"}, the identifier of an object which this object replaces.}
#'    \item{\code{obsoletedBy}:}{value of type \code{"character"}, the identifier of an object that replaces this object.}
#'    \item{\code{archived}:}{value of type \code{"logical}, a boolean flag indicating whether the object has been archived and thus hidden.}
#'    \item{\code{dateUploaded}:}{value of type \code{"character"}, the date on which the object was uploaded to a member node.}
#'    \item{\code{dateSysMetadataModified}:}{value of type \code{"character"}, the last date on which this system metadata was modified.}
#'    \item{\code{originMemberNode}:}{value of type \code{"character"}, the node identifier of the node on which the object was originally registered.}
#'    \item{\code{authoritativeMemberNode}:}{value of type \code{"character"}, the node identifier of the node which currently is authoritative for the object.}
#'  }
#'
#' Construct SystemMetadata, with all fields as null
#' @return the SystemMetadata object representing an object
#' @author jones
## @export
setGeneric(name = "SystemMetadata", def = function(x) {
  standardGeneric("SystemMetadata")
})


#' @rdname SystemMetadata-method
#' @aliases SystemMetadata
setMethod("SystemMetadata", signature(), function(x) {

	## create new SystemMetadata object
	sysmeta <- new("SystemMetadata")
    sysmeta@serialVersion <- 1
	return(sysmeta)
})

## TODO: Constructor that  takes XML as input
## Construct a SystemMetadata, with all fields as null
## @returnType SystemMetadata  
## @return the SystemMetadata object representing an object
## 
## @author jones
## @export
#setMethod("SystemMetadata", signature("XMLInternalElementNode"), function(x) {
#    
#    ## create new SystemMetadata object, and parse the XML to populate fields
#    sysmeta <- new("SystemMetadata")
#    sysmeta <- parseSystemMetadata(x)
#    return(sysmeta)
#})

##########################
## Methods
##########################

## TODO: method to parse SystemMetadata from XML


#' @description
#' Parse an XML representation of system metadata, and set the object slots with obtained values.
#' @param sysmeta the \code{SystemMetadata} object
#' @param xml the XML representation of the capabilities, as an XMLInternalElementNode
#' @param ... additional arguments passed to ??? (where are these passed to?)
#' @return the SystemMetadata object representing an object
#' 
#' @author jones
#' @export
setGeneric("parseSystemMetadata", function(sysmeta, xml, ...) {
    standardGeneric("parseSystemMetadata")
})

#' @rdname parseSystemMetadata-methods
#' @aliases parseSystemMetadata, SystemMetadata-method
setMethod("parseSystemMetadata", signature("SystemMetadata", "XMLInternalElementNode"), function(sysmeta, xml) {

    sysmeta@serialVersion <- as.numeric(xmlValue(xml[["serialVersion"]]))
    sysmeta@identifier <- xmlValue(xml[["identifier"]])
    sysmeta@formatId <- xmlValue(xml[["formatId"]])
    sysmeta@size <- as.numeric(xmlValue(xml[["size"]]))
    sysmeta@checksum <- xmlValue(xml[["checksum"]])
    csattrs <- xmlAttrs(xml[["checksum"]])
    sysmeta@checksumAlgorithm <- csattrs[[1]]
    sysmeta@submitter <- xmlValue(xml[["submitter"]])
    sysmeta@rightsHolder <- xmlValue(xml[["rightsHolder"]])
    accessList <- xmlChildren(xml[["accessPolicy"]])
    for (accessNode in accessList) {
        nodeName <- xmlName(accessNode)
        if (grepl("allow", nodeName)) {
            accessRule <- list()
            nodeList <- xmlChildren(accessNode)
            subjects <- list()
            permissions <- list()
            for (node in nodeList) {
                nodeName <- xmlName(node)
                if (grepl("subject", nodeName)) {
                    accessRule <- lappend(accessRule, xmlValue(node))
                    subjects <- lappend(subjects, xmlValue(node))
                } else if (grepl("permission", nodeName)) {
                    accessRule <- lappend(accessRule, xmlValue(node))
                    permissions <- lappend(permissions, xmlValue(node))
                }
            }
            for (subj in subjects) {
                for (perm in permissions) {
                    accessRecord <- data.frame(subject=subj, permission=perm)
                    sysmeta@accessPolicy <- rbind(sysmeta@accessPolicy, accessRecord)
                }
            }
        }
    }
    rpattrs <- xmlAttrs(xml[["replicationPolicy"]])
    repAllowed <- grep('true', rpattrs[["replicationAllowed"]], ignore.case=TRUE)
    if (repAllowed) {
        sysmeta@replicationAllowed = TRUE
        sysmeta@numberReplicas = as.numeric(rpattrs[["numberReplicas"]])
        pbMNList <- xmlChildren(xml[["replicationPolicy"]])
        for (pbNode in pbMNList) {
            nodeName <- xmlName(pbNode)
            if (grepl("preferredMemberNode", nodeName)) {
                sysmeta@preferredNodes <- lappend(sysmeta@preferredNodes, xmlValue(pbNode))
            } else if (grepl("blockedMemberNode", nodeName)) {
                sysmeta@blockedNodes <- lappend(sysmeta@blockedNodes, xmlValue(pbNode))
            }
        }
    }
    sysmeta@obsoletes <- xmlValue(xml[["obsoletes"]])
    sysmeta@obsoletedBy <- xmlValue(xml[["obsoletedBy"]])
    sysmeta@archived <- as.logical(xmlValue(xml[["archived"]]))
    sysmeta@dateUploaded <- xmlValue(xml[["dateUploaded"]])
    sysmeta@dateSysMetadataModified <- xmlValue(xml[["dateSysMetadataModified"]])
    sysmeta@originMemberNode <- xmlValue(xml[["originMemberNode"]])
    sysmeta@authoritativeMemberNode <- xmlValue(xml[["authoritativeMemberNode"]])
    #TODO: sysmeta@replica    
    
    return(sysmeta)
})

# TODO: method to serialize XML from object

#' @description
#' Serialize an XML representation of system metadata.
#' @param sysmeta the SystemMetadata instance to be serialized
#' @return the character string representing a SystemMetadata object
#' @author jones 
#' @export
setGeneric("serialize", function(sysmeta, ...) {
    standardGeneric("serialize")
})
#' @rdname serialize-methods
#' @aliases serialize, SystemMetadata-method
setMethod("serialize", signature("SystemMetadata"), function(sysmeta) {
    
    root <- xmlNode("systemMetadata",
                      namespace="d1",
                      namespaceDefinitions = c(d1 = "http://ns.dataone.org/service/types/v1"))
    root <- addChildren(root, xmlNode("serialVersion", sysmeta@serialVersion))
    root <- addChildren(root, xmlNode("identifier", sysmeta@identifier))
    root <- addChildren(root, xmlNode("formatId", sysmeta@formatId))
    root <- addChildren(root, xmlNode("size", sysmeta@size))
    root <- addChildren(root, xmlNode("checksum", sysmeta@checksum, attrs = c(algorithm = sysmeta@checksumAlgorithm)))
    root <- addChildren(root, xmlNode("submitter", sysmeta@submitter))
    root <- addChildren(root, xmlNode("rightsHolder", sysmeta@rightsHolder))

    if (nrow(sysmeta@accessPolicy) > 0) {
        accessPolicy <- xmlNode("accessPolicy")
        for(i in 1:nrow(sysmeta@accessPolicy)) {
            accessRule <- xmlNode("allow")
            accessRule <- addChildren(accessRule, xmlNode("subject", sysmeta@accessPolicy[i,]$subject))
            accessRule <- addChildren(accessRule, xmlNode("permission", sysmeta@accessPolicy[i,]$permission))
            accessPolicy <- addChildren(accessPolicy, accessRule)
        }
        root <- addChildren(root, accessPolicy)
    }
    
    if (!is.null(sysmeta@replicationAllowed)) {
        rpolicy <- xmlNode("replicationPolicy", attrs = c(replicationAllowed=tolower(as.character(sysmeta@replicationAllowed)), numberReplicas=sysmeta@numberReplicas))
        pnodes <- lapply(sysmeta@preferredNodes, xmlNode, name="preferredMemberNode")
        bnodes <- lapply(sysmeta@blockedNodes, xmlNode, name="blockedMemberNode")
        rpolicy <- addChildren(rpolicy, kids=c(pnodes, bnodes))
        root <- addChildren(root, rpolicy)
    }
    if (!is.na(sysmeta@obsoletes)) {
        root <- addChildren(root, xmlNode("obsoletes", sysmeta@obsoletes))
    }
    if (!is.na(sysmeta@obsoletedBy)) {
        root <- addChildren(root, xmlNode("obsoletedBy", sysmeta@obsoletedBy))
    }
    root <- addChildren(root, xmlNode("archived", tolower(as.character(sysmeta@archived))))
    root <- addChildren(root, xmlNode("dateUploaded", sysmeta@dateUploaded))
    root <- addChildren(root, xmlNode("dateSysMetadataModified", sysmeta@dateSysMetadataModified))
    root <- addChildren(root, xmlNode("originMemberNode", sysmeta@originMemberNode))
    root <- addChildren(root, xmlNode("authoritativeMemberNode", sysmeta@authoritativeMemberNode))
    #TODO: sysmeta@replica (but not really needed for anything, so low priority)

    xml <- saveXML(root, encoding="UTF-8")  # NB: Currently saveXML ignores the encoding parameter
    
    return(xml)
})

lappend <- function(lst, obj) {
    lst[[length(lst)+1]] <- obj
    return(lst)
}

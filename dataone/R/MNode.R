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

#' MNode is a class representing a Member Node repository within the DataONE federation of data repositories.
#' @description MNode provides functions interacting with the a DataONE Member Node repository, which
#' is a repository that provides access for reading and writing data and metadata using the common
#' DataONE service API.  The MNode API includes functions for retrieving data and metadata based on its
#' unique persistent identifier (pid), as well as for creating, updating, and archiving these data and
#' metadata objects.  
#' @details   
#' Methods that perform write operations on the Member Node generally require
#' authentication, which is managed via a client-side X.509 certificate via
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' @slot endpoint The url to access node services, which is the baseURL plus the version string
#' @author Matthew Jones
#' @rdname MNode-class
#' @include D1Node.R
#' @keywords classes
#' @exportClass MNode
#' @examples
#' \dontrun{
#' cn <- CNode("STAGING2")
#' mn <- getMNode(cn, "urn:node:mnTestKNB")
#' mnid <- mn@@identifier
#' newid <- generateIdentifier(mn, "UUID")
#' cm <- CertificateManager()
#' u <- showClientSubject(cm)
#' testdf <- data.frame(x=1:10,y=11:20)
#' csvfile <- paste(tempfile(), ".csv", sep="")
#' write.csv(testdf, csvfile, row.names=FALSE)
#' f <- "text/csv"
#' size <- file.info(csvfile)$size
#' sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
#' sysmeta <- new("SystemMetadata", identifier=newid, formatId=f, size=size, submitter=u, rightsHolder=u, checksum=sha1, originMemberNode=mnid, authoritativeMemberNode=mnid)
#' response <- create(mn, newid, csvfile, sysmeta)
#' response <- archive(mn, newid)
#' }
setClass("MNode", slots = c(endpoint = "character"), contains="D1Node")

#########################
## MNode constructors
#########################

#' Create a MNode object representing a DataONE Member Node repository.
#' @description Construct an instance of MNode to provide mechanisms to access, create, and update data and 
#' metadata objects on the associated Member Node.
#' @details If the \code{'x'} is a string, it is treated as a URI and an attempt to find an associated
#' Member Node at that base URL is attempted.  If \code{'x'} is a Node reference, then it is cast to a MNode
#' instance.  This typically is used from the getMNode() function from the CNode class, which is the preferred
#' way to retrieve an instance of an MNode.
#' @param x a URI representing a node base URL, or a reference to a dataone::Node instance
#' @return the MNode object
#' @export
setGeneric("MNode", function(x) {
  standardGeneric("MNode")
})

#' @describeIn MNode
setMethod("MNode", signature("character"), function(x) {

	## create new MNode object and insert uri endpoint
	mnode <- new("MNode")
	mnode@endpoint <- x

	## Lookup the rest of the node information
	xml <- getCapabilities(mnode)
  # getCapabilties returns NULL if an error was encoutered
  if (is.null(xml)) return(NULL)
  mnode <- parseCapabilities(mnode, xmlRoot(xml))
	# Set the service URL fragment for the solr query engine
	mnode@serviceUrls <- data.frame(service="query.solr", Url=paste(mnode@endpoint, "query", "solr/", sep="/"), row.names = NULL, stringsAsFactors = FALSE)
	return(mnode)
})

#' @describeIn MNode
setMethod("MNode", signature("D1Node"), function(x) {
  
  if (x@type == "mn") {
    ## create new MNode object and insert uri endpoint
    mnode <- new("MNode")
    mnode@identifier = x@identifier
    mnode@name = x@name    
    mnode@description = x@description
    mnode@baseURL = x@baseURL
    mnode@subject = x@subject
    mnode@contactSubject = x@contactSubject
    mnode@replicate = x@replicate
    mnode@type = x@type
    mnode@state = x@state
    mnode@endpoint <- paste(x@baseURL, "v1", sep="/")
    # Set the service URL fragment for the solr query engine
    mnode@serviceUrls <- data.frame(service="query.solr", Url=paste(mnode@endpoint, "query", "solr/", sep="/"), row.names = NULL, stringsAsFactors = FALSE)
    return(mnode)
  } else {
    return(NULL)
  }
})

##########################
## Methods
##########################

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.ping
# public Date ping() 
 
#' Get the node capabilities description, and store the information in the MNode.
#' @description Access the DataONE getCapabilities() service for the Member Node, which returns an XML
#' decription of the repository and the services it offers.
#' @param mnode The node identifier with which this node is registered in DataONE
#' @return the MNode object representing the DataONE environment
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_core.getCapabilities}
#' @import XML
#' @import httr
#' @export
setGeneric("getCapabilities", function(mnode, ...) {
    standardGeneric("getCapabilities")
})

#' @describeIn MNode
setMethod("getCapabilities", signature("MNode"), function(mnode) {
  url <- paste(mnode@endpoint, "node", sep="/")
	response <- GET(url, user_agent(mnode@userAgent))
  if(response$status != "200") {
    cat(sprintf("Error accessing %s: %s\n", mnode@endpoint, getErrorDescription(response)))
		return(NULL)
 	}
	xml <- content(response)
	return(xml)
})

#' Get the bytes associated with an object on this Member Node.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The MNode instance from which the pid will be downloaded
#' @param pid The identifier of the object to be downloaded
#' @param check Check if the requested pid has been obsoleted and print a warning if true
#' @return the bytes of the object
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNRead.get}
#' @export
#' @describeIn MNode
setMethod("get", signature("MNode", "character"), function(node, pid, check=as.logical(FALSE)) {
  
    if(!class(check) == "logical") {
      stop("Invalid argument: 'check' must be specified as a logical.")
    }

    # TODO: need to properly URL-escape the PID
    url <- paste(node@endpoint, "object", pid, sep="/")
    
    # Check if the requested pid has been obsoleted by a newer version
    # and print a warning
    if (check) {
      sysmeta <- getSystemMetadata(node, pid)
      if (!is.na(sysmeta@obsoletedBy)) {
        message(sprintf('Warning: pid "%s" is obsoleted by pid "%s"', pid, sysmeta@obsoletedBy))
      }
    }
    
    # Use an authenticated connection if a certificate is available 
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    response <- NULL
    if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
        response <- GET(url, config=config(sslcert = cert), user_agent(node@userAgent))
    } else {
        response <- GET(url, user_agent(node@userAgent))   # the anonymous access case
    }
	
    if(response$status != "200") {
      cat(sprintf("get() error: %s\n", getErrorDescription(response)))
      #d1_errors(response)
		  return(NULL)
  	}
 	return(content(response, as="raw"))
})

#' Get the metadata describing system properties associated with an object on this Member Node.
#' @description The SystemMetadata includes information about the identity, type, access control, and other system
#' level details about the object.
#' @details This operation acts as the 'public' anonymous user unless an X.509 certificate is
#' present in the default location of the file system, in which case the access will be authenticated.
#' @param node The MNode instance from which the SystemMetadata will be downloaded
#' @param pid The identifier of the object
#' @return SystemMetadata for the object
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNRead.getSystemMetadata}
#' @import datapackage
#' @export
#' @describeIn MNode
setMethod("getSystemMetadata", signature("MNode", "character"), function(node, pid) {
    # TODO: need to properly URL-escape the PID
    url <- paste(node@endpoint, "meta", pid, sep="/")
    # Use an authenticated connection if a certificate is available
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    response <- NULL
    if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
        response <- GET(url, config=config(sslcert = cert), user_agent(node@userAgent))
    } else {
        response <- GET(url, user_agent(node@userAgent))
    }
    if(response$status != "200") {
        return(NULL)
    }
    # Convert the response into a SystemMetadata object
    sysmeta <- SystemMetadata(xmlRoot(content(response)))
    return(sysmeta)
})

#' This method provides a lighter weight mechanism than getSystemMetadata() for a client to
#' determine basic properties of the referenced object.
#' @param mnode The MNode instance from which the identifier will be generated
#' @param pid Identifier for the object in question. May be either a PID or a SID. Transmitted as
#' part of the URL path and must be escaped accordingly.
#' @return A list of header elements
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNRead.describe}
#' @examples \dontrun{
#' mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
#' mn <- MNode(mn_uri)
#' pid <- "knb.473.1"
#' describe(mn, pid)
#' describe(mn, "adfadf") # warning message when wrong pid
#' }
#' @describeIn MNode
#' @export
#' @author Scott Chamberlain
setMethod("describe", signature("MNode", "character"), function(node, pid) {
  url <- file.path(node@endpoint, "object", pid)
  response <- HEAD(url)
  if(response$status != "200") {
    d1_errors(response)
  } else { return(unclass(response$headers)) }
})
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.getChecksum
# public Checksum getChecksum(Identifier pid, String checksumAlgorithm)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.listObjects
# public ObjectList listObjects(Date fromDate, Date toDate, ObjectFormatIdentifier formatid, Boolean replicaStatus, Integer start, Integer count) 

#' Create an object on a Member Node.
#' @description This method provides the ability to upload a data or metadata object to the Member Node
#' provided in the \code{'mnode'} parameter.  
#' @details This operation requires an X.509 certificate to be present in the default location of the file 
#' system. This certificate provides authentication credentials from 
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' @param node The MNode instance on which the object will be created
#' @param pid The identifier of the object to be created
#' @param filepath the absolute file location of the object to be uploaded
#' @param sysmeta a SystemMetadata instance describing properties of the object
#' @return XML describing the result of the operation, including the identifier if successful
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.create}
#' @import datapackage
#' @export
setGeneric("create", function(mnode, pid, ...) {
    standardGeneric("create")
})

#' @describeIn MNode
setMethod("create", signature("MNode", "character"), function(mnode, pid, filepath, sysmeta) {
    # TODO: need to properly URL-escape the PID
    url <- paste(mnode@endpoint, "object", sep="/")
    # Use an authenticated connection if a certificate is available
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    response <- NULL
    if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
        sysmetaxml <- serializeSystemMetadata(sysmeta)
        sm_file <- tempfile()
        writeLines(sysmetaxml, sm_file)
        response <- POST(url, encode="multipart", body=list(pid=pid, object=upload_file(filepath), 
                    sysmeta=upload_file(sm_file, type='text/xml')), 
                    config=config(sslcert = cert), user_agent(mnode@userAgent))
    } else {
        # This is an error, one must be authenticated
        show_auth_message()
        return(NULL)
    }
    if(response$status != "200") {
        #d1_errors(response)
        cat(sprintf("Error updating %s: %s\n", pid, getErrorDescription(response)))
        return(NULL)
    } else {
        return(content(response))
    }
})

#' Update an object on a Member Node, by creating a new object that replaces an original.
#' @description This method provides the ability to update a data or metadata object to the Member Node
#' provided in the \code{'mnode'} parameter.  In DataONE, both the original object and the new object are
#' maintained, each with its own persistent identifier, and the 'obsoletes' field in the SystemMetadata is
#' used to reflect the fact that the new object replaces the old.  Both objects remain accessible.
#' @details This operation requires an X.509 certificate to be present in the default location of the file 
#' system. This certificate provides authentication credentials from 
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' @param node The MNode instance on which the object will be created
#' @param pid The identifier of the object to be updated
#' @param filepath the absolute file location of the object to be uploaded
#' @param newpid The identifier of the new object to be created
#' @param sysmeta a SystemMetadata instance describing properties of the object
#' @return XML describing the result of the operation, including the identifier if successful
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.update}
#' @import datapackage
#' @export
setGeneric("update", function(mnode, pid, ...) {
    standardGeneric("update")
})

#' @describeIn MNode
setMethod("update", signature("MNode", "character"), function(mnode, pid, filepath, newpid, sysmeta) {
    # TODO: need to properly URL-escape the PID
    url <- paste(mnode@endpoint, "object", sep="/")
    # Use an authenticated connection if a certificate is available
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    response <- NULL
    if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
        sysmetaxml <- serializeSystemMetadata(sysmeta)
        sm_file <- tempfile()
        writeLines(sysmetaxml, sm_file)
        response <- PUT(url, encode="multipart", body=list(pid=pid, object=upload_file(filepath), 
                        newPid=newpid, sysmeta=upload_file(sm_file, type='text/xml')), 
                        config=config(sslcert = cert), user_agent(mnode@userAgent))
    } else {
        # This is an error, one must be authenticated
        show_auth_message()
        return(NULL)
    }
    if(response$status != "200") {
        d1_errors(response)
        return(NULL)
    } else {
        return(content(response))
    }
})

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_storage.delete
# public Identifier delete(Identifier pid)

#' Archive an object on a Member Node, which hides it from casual searches.
#' @description This method provides the ability to archive a data or metadata object on the Member Node
#' provided in the \code{'mnode'} parameter.  Archiving removes the object from DataONE search functions,
#' thereby making it more difficult to find without completely removing the object.  Archive is intended
#' for objects that should not be used by current researchers, but for which there is a desire to maintain
#' a historical record, such as when journal articles might cite the object.  Users can still obtain the
#' contents of archived objects if they have the identifier, but will not discover it through searches.
#' @details This operation requires an X.509 certificate to be present in the default location of the file 
#' system. This certificate provides authentication credentials from 
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' @param node The MNode instance on which the object will be created
#' @param pid The identifier of the object to be created
#' @return XML describing the result of the operation, inlcuding the identifier if successful
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.archive}
#' @export
setGeneric("archive", function(mnode, pid, ...) {
    standardGeneric("archive")
})

#' @describeIn MNode
setMethod("archive", signature("MNode", "character"), function(mnode, pid) {
    # TODO: need to properly URL-escape the PID
    url <- paste(mnode@endpoint, "archive", pid, sep="/")
    # Use an authenticated connection if a certificate is available
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    response <- NULL
    if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
        response <- PUT(url, config=config(sslcert = cert), user_agent(mnode@userAgent))
    } else {
        # This is an error, one must be authenticated
        show_auth_message()
        return(NULL)
    }
    if(response$status != "200") {
        d1_errors(response)
        return(NULL)
    } else {
        return(content(response))
    }
})

#' Get a unique identifier that is generated by the Member Node repository and guaranteed to be unique.
#' @description Creating objects requires use of a unique persistent identifier (pid) when calling the create
#' function.  Member Nodes may optionally provide the generateIdentifier service to issue such identifiers, 
#' ensuring that they are unique. Each identifier conforms to an identifier scheme, which determines the syntax and
#' rules for how the identifier that is generated is formatted.  All Member Nodes that implement this method must 
#' support the UUID scheme, but may also support other schemes such as DOI and others.
#' @details This operation requires an X.509 certificate to be present in the default location of the file 
#' system. This certificate provides authentication credentials from 
#' CILogon \url{https://cilogon.org/?skin=DataONE}.  See \code{\link{{CertificateManager}}} for details.
#' @param mnode The MNode instance on which the object will be created
#' @param scheme The identifier scheme to be used, such as DOI, UUID, etc.
#' @param fragment An optional fragment to be prepended to the identifier for schemes that support it (not all do).
#' @return the character string of the unique identifier
#' @seealso \url{http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNStorage.generateIdentifier}
#' @export
setGeneric("generateIdentifier", function(mnode, ...) {
    standardGeneric("generateIdentifier")
})

#' @describeIn MNode
setMethod("generateIdentifier", signature("MNode"), function(mnode, scheme="UUID", fragment=NULL) {
    # TODO: need to properly URL-escape the PID
    url <- paste(mnode@endpoint, "generate", sep="/")
    cm = CertificateManager()
    cert <- getCertLocation(cm)
    body = list(scheme = scheme, fragment = fragment)
    if (is.null(fragment)) {
        body = list(scheme = scheme)
    }
    response <- POST(url = url, body = body, encode="multipart", config=config(sslcert = cert), user_agent(mnode@userAgent))
    if(response$status != "200") {
        return(NULL)
    }
    # convert the response into a character string
    xml <- content(response)
    new_identifier <- xmlValue(xmlRoot(xml))
    return(new_identifier)
})

#' Upload a DataPackage to a DataONE member node
#' 
#' @description Upload all DataObjects contained in the DataPackage by calling uploadDataObject()
#' on each of the members. Also a resourceMap object is create from the
#' recorded relationships between DataObjects, and this is uploaded as well.
#' 
#' @details The DataPackage describes the collection of data object and their associated 
#' metadata object, with the relationships and members serialized into a document
#' stored under, and retrievable with, the packageId as it's own distinct object.
#' 
#' Members are created serially, and most errors in creating one object will 
#' interrupt the create process for the whole, with the result that some members will 
#' be created, and the remainder not.
#' 
#' @param mn An MNode object that represents the member node to upload the package to.
#' @param dataPackage The DataPackage instance to be submitted to DataONE for creation.
#' @param replicate A value of type \code{"logical"}, if TRUE then DataONE will replicate this object to other member nodes
#' @param numberReplicas A value of type \code{"numeric"}, for number of supported replicas.
#' @param preferredNodes A list of \code{"character"}, each of which is the node identifier for a node to which a replica should be sent.
#' @param public A \code{'logical'}, if TRUE then all objects in this package will be accessible by any user
#' @param accessRules Access rules of \code{'data.frame'} that will be added to the access policy
#' @return id The identifier of the resource map for this data package
#' @import datapackage
#' @import uuid
#' @export
setGeneric("uploadDataPackage", function(mn, dp, ...) {
  standardGeneric("uploadDataPackage")
})

#' @describeIn MNode
setMethod("uploadDataPackage", signature("MNode", "DataPackage"), function(mn, dp, replicate=NA, numberReplicas=NA, preferredNodes=NA,  public=as.logical(FALSE), accessRules=NA, ...) {
  
  submitter <- as.character(NULL)
  # Upload each object that has been added to the DataPackage
  for (doId in getIdentifiers(dp)) {
    do <- getMember(dp, doId)
    
    submitter <- do@sysmeta@submitter
    if (public) {
      do <- setPublicAccess(do)
    }
    uploadDataObject(mn, do, replicate, numberReplicas, preferredNodes, public, accessRules)
  }
  
  # Create a resource map for this DataPackage and upload it
  tf <- tempfile()
  serializationId <- paste0("urn:uuid:", UUIDgenerate())
  status <- serializePackage(dp, tf, id=serializationId)
  resMapObj <- new("DataObject", id=serializationId, format="http://www.openarchives.org/ore/terms", user=submitter, mnNodeId=mn@identifier, filename=tf)
  returnId <- uploadDataObject(mn, resMapObj, replicate, numberReplicas, preferredNodes, public, accessRules)
  return(returnId)
})

#' Upload a DataObject to a DataONE repository
#' @param mn An MNode object that represents the member node to upload the package to.
#' @param do The DataObject instance to be uploaded to DataONE.
#' @param public A \code{'logical'}, if TRUE then all objects in this package wil be accessible by any user
#' @param replicate A value of type \code{"logical"}, if TRUE then DataONE will replicate this object to other member nodes
#' @param accessRules Access rules of \code{'data.frame'} that will be added to the access policy
#' @param numberReplicas A value of type \code{"numeric"}, for number of supported replicas.
#' @param preferredNodes A list of \code{"character"}, each of which is the node identifier for a node to which a replica should be sent.
#' @return id The id of the DataObject that was uploaded
#' @import datapackage
#' @export
setGeneric("uploadDataObject", function(mn, do, ...) {
  standardGeneric("uploadDataObject")
})

#' @describeIn MNode
setMethod("uploadDataObject", signature("MNode", "DataObject"), 
                     function(mn, do, replicate=as.logical(FALSE), numberReplicas=NA, preferredNodes=NA, public=as.logical(FALSE), accessRules=NA, ...)  {
  
  # Ensure the user is logged in before the upload
  cm <- CertificateManager()
  user <- showClientSubject(cm)
  
  if (isCertExpired(cm)) {
    stop("Unable to upload data, your certificate expired on: ", getCertExpires(cm))
  }

  doId <- do@sysmeta@identifier
  # Set sysmeta values if passed in and not already set in sysmeta for each data object
  if (!is.na(replicate)) {
    do@sysmeta@replicationAllowed <- as.logical(replicate)
  }
  if (!is.na(numberReplicas)) {
    do@sysmeta@numberReplicas <- as.numeric(numberReplicas)
  }
  if (!all(is.na(preferredNodes))) {
    do@sysmeta@preferredNodes <- as.list(preferredNodes)
  }
  
  if (public) {
    do@sysmeta <- addAccessRule(do@sysmeta, "public", "read")
  }
  
  # addAccessRule will add all rules (rows) in accessRules in one call
  if(!all(is.na(accessRules))) {
    do@sysmeta <- addAccessRule(do@sysmeta, accessRules)
  }
  sysmetaXML <- serializeSystemMetadata(do@sysmeta)
  # Upload the data to the MN using create(), checking for success and a returned identifier
  createdId <- create(mn, doId, do@filename, do@sysmeta)
  #    if (is.null(createdId) | !grepl(newid, xmlValue(xmlRoot(createdId)))) {
  if (is.null(createdId) || !grepl(doId, xmlValue(xmlRoot(createdId)))) {
    #warning(paste0("Error on returned identifier: ", createdId))
    return(NULL)
  } else {
    return(doId)
  }
})

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNQuery.getQueryEngineDescription
# public QueryEngineDescription getQueryEngineDescription(String queryEngine)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MNQuery.listQueryEngines
# public QueryEngineList listQueryEngines()


############# Private functions, internal to this class, not for external callers #################

show_auth_message <- function() {
    message(sprintf('Exception name: %s', "NotAuthenticated"), "\n")
    message(sprintf('Exception description: %s', "You must be logged in with a valid certificate file."), "\n")
    message("You can log in and download a certificate at https://cilogon.org/?skin=DataONE")
}

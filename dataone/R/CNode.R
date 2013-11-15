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

## 
## @slot endpoint The baseurl of the CN in this environment
## @slot client The reference to the internally held Java D1Client instance
## @slot session The reference to the internally held Java Session instance
## @author jones
## @export
setClass("CNode",
         representation(endpoint = "character",
                        client = "jobjRef",
                        session = "jobjRef")
)

#########################
## CNode constructors
#########################

## @param env The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV')
## @param ... (not yet used)
## @returnType CNode  
## @return the CNode object representing the DataONE environment
## 
## @author jones
## @export
setGeneric("CNode", function(env, ...) {
  standardGeneric("CNode")
})

## 
## Construct a CNode, using default env ("PROD")
## @name CNode
## 
## @returnType CNode  
## @return the CNode object representing the DataONE environment
## 
## @author jones
## @docType methods
## @export
setMethod("CNode", , function() {
    result <- CNode("PROD")
    return(result)
})

## Pass in the environment to be used by this D1Client, plus the 
## id of the member node to be used for primary interactions such as creates
setMethod("CNode", signature("character"), function(env) {

  ## Define the default CNs for each environment.
  PROD <- "https://cn.dataone.org/cn"
  STAGING <- "https://cn-stage.test.dataone.org/cn"
  STAGING2 <- "https://cn-stage-2.test.dataone.org/cn"
  SANDBOX <- "https://cn-sandbox.test.dataone.org/cn"
  DEV <- "https://cn-dev.test.dataone.org/cn"

  # By default, use production.  But also look in the environment.
  CN_URI <- PROD
  cn.url <- Sys.getenv("CN_URI")
  if(cn.url != "") {
    CN_URI <- cn.url
  } else {
    if (env == "DEV") CN_URI <- DEV
    if (env == "STAGING") CN_URI <- STAGING
    if (env == "STAGING2") CN_URI <- STAGING2
    if (env == "SANDBOX") CN_URI <- SANDBOX
    if (env == "PROD") CN_URI <- PROD
  }

  config <- J("org/dataone/configuration/Settings")$getConfiguration()
  config$setProperty("D1Client.CN_URL", CN_URI)

  ## create new D1Client object and insert uri endpoint
  result <- new("CNode")
  result@endpoint <- CN_URI

  ## an expired certificate can crash the R session, so want to check before 
  ## instantiating any Java objects, which might interact with the DataONE environment
  ## while setting things up.  (It will be called in this routine when 
  ## validating the member node id)
  cm <- CertificateManager()
  if (isCertExpired(cm)) {
      message("Your client certificate is expired.  Please download new one before continuing...")
      return(NULL)
  }
  
  ## Create a Java D1Client object to use for contacting the server
  client <- .jnew("org/dataone/client/D1Client") 
  result@client <- client
  
  ## initialize the session, but not sure where it's used
  result@session <- .jnull("org/dataone/service/types/v1/Session") 

  return(result)
})

##########################
## Methods
##########################

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CN_core.ping
# public Date ping()
	
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.listFormats
# public ObjectFormatList listFormats()

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.getFormat
# public ObjectFormat getFormat(ObjectFormatIdentifier formatid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.getChecksumAlgorithms
# public ChecksumAlgorithmList listChecksumAlgorithms()

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.getLogRecords
# public Log getLogRecords(Date fromDate, Date toDate, Event event, String pidFilter, Integer start, Integer count) 

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.listNodes
# public NodeList listNodes()

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.reserveIdentifier
# public Identifier reserveIdentifier(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.generateIdentifier
# public Identifier generateIdentifier(String scheme, String fragment)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.hasReservation
# public boolean hasReservation(Subject subject, Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.setObsoletedBy
# public boolean setObsoletedBy(Identifier pid, Identifier obsoletedByPid, long serialVersion)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.delete
# public Identifier delete(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNCore.archive
# public Identifier archive(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.get
# public InputStream get(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getSystemMetadata
# public SystemMetadata getSystemMetadata(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/MN_APIs.html#MN_read.describe
# public DescribeResponse describe(Identifier pid)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.resolve
# public ObjectLocationList resolve(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getChecksum
# public Checksum getChecksum(Identifier pid)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.listObjects
# public ObjectList listObjects(Date fromDate, Date toDate, ObjectFormatIdentifier formatId, Boolean replicaStatus, Integer start, Integer count) 
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.search
# public ObjectList search(String queryType, String query)
    
# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.query
# public InputStream query(String queryEngine, String query)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.getQueryEngineDescription
# public QueryEngineDescription getQueryEngineDescription(String queryEngine)

# @see http://mule1.dataone.org/ArchitectureDocs-current/apis/CN_APIs.html#CNRead.listQueryEngines
# public QueryEngineList listQueryEngines()

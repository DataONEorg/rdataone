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
## @slot endpoint The baseurl of the CN in that environment
## @slot mn.nodeid The NodeReference for the 'home' MemberNode for this application, where creates/updates will happen.
## @slot client The reference to the internally held Java D1Client instance
## @slot session The reference to the internally held Java Session instance
## @author rnahf
## @export
setClass("D1Client",
         representation(endpoint = "character",
                        mn.nodeid = "character",
                        client = "jobjRef",
                        session = "jobjRef")
)

#########################
## D1Client constructors
#########################

## Generic function with 0, 1, or 2 parameters
## 
## @param env The label for the DataONE environment to be using ('PROD','STAGING','SANDBOX','DEV')
## @param mn_nodeid The node Id of the application's 'home' node.  Should be already registered to the corresponding 'env'
## @param ... (not yet used)
## @returnType D1Client  
## @return the D1Client object representing the DataONE environment
## 
## @author mbjones
## @export
setGeneric("D1Client", function(env, mnNodeid, ...) {
  standardGeneric("D1Client")
})

## 
## Construct a D1Client, using default env ("PROD") and nodeid ("")
## @name D1Client
## @alias D1Client,-method
## 
## @returnType D1Client  
## @return the D1Client object representing the DataONE environment
## 
## @author rnahf
## @docType methods
## @export
setMethod("D1Client", , function() {
    result <- D1Client("PROD", "")
    return(result)
})

## Pass in the environment to be used by this D1Client, but use
##   the default member node.
setMethod("D1Client", signature("character"), function(env, ...) {
    message("instantiating D1Client without a 'home' Member Node...")
    result <- D1Client(env, "")
    return(result)
})

## Pass in the environment to be used by this D1Client, plus the 
## id of the member node to be used for primary interactions such as creates
setMethod("D1Client", signature("character", "character"), function(env, mnNodeid) {

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
  result <- new("D1Client")
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
  
  ## Check and set the node reference
  if (mnNodeid == "") {
    ## allow the mn to be unset with empty string only
    result@mn.nodeid <- mnNodeid
  } else {
    ## check to see if it's a valid value   
    jNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
    jNodeRef$setValue(mnNodeid)
  
    mnUrl <- result@client$getCN()$lookupNodeBaseUrl(mnNodeid)
    if (is.null(mnUrl) || mnUrl == "") {
        message("Error: The provided mnNodeid value is invalid for the DataONE environment")
        return(NULL)
    } else {
        result@mn.nodeid <- mnNodeid
    }
  }
 
  ## initialize the session, but not sure where it's used
  result@session <- .jnull("org/dataone/service/types/v1/Session") 

  return(result)
})


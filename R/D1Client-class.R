#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2012
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

setClass("D1Client",
         representation(endpoint = "character",
	 		mnRef = "jobjRef",
                        client = "jobjRef",
                        session = "jobjRef")
)

#####################
## D1Client constructors
#####################

## generic
setGeneric("D1Client", function(...) { standardGeneric("D1Client")} )

## no arguments in the signature
setMethod("D1Client", ,
    function() {
    result <- new("D1Client", "PROD")
    return(result)
})

## Pass in the environment to be used by this D1Client
setMethod("D1Client", ,
    function(env) {

    ## Select which CN environment should be used
    PROD <- "https://cn.dataone.org/cn"
    STAGING <- "https://cn-stage.test.dataone.org/cn"
    SANDBOX <- "https://cn-sandbox.test.dataone.org/cn"
    DEV <- "https://cn-dev.test.dataone.org/cn"
 
    CN_URI <- PROD
    cnUrl <- Sys.getenv("CN_URI")
    if(cnUrl != "") {
        CN_URI <- cnUrl
    } else {
	if (env == "DEV") CN_URI <- DEV
	if (env == "STAGING") CN_URI <- STAGING
	if (env == "SANDBOX") CN_URI <- SANDBOX
	if (env == "PROD") CN_URI <- PROD
    }
 
    config <- J("org/dataone/configuration/Settings")$getConfiguration()
    config$setProperty("D1Client.CN_URL", CN_URI)

    ## create new D1Client object and insert uri endpoint
    result <- new("D1Client")
    result@endpoint <- CN_URI
    result@mnRef <- .jnull("org/dataone/service/types/v1/NodeReference")
    
    ## Create a Java D1Client object to use for contacting the server
    client <- .jnew("org/dataone/client/D1Client") 
    result@client <- client
    result@session <- .jnull("org/dataone/service/types/v1/Session") 

    return(result)
})

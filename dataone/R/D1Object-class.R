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

setClass("D1Object", representation(jD1o = "jobjRef") )

#####################
## D1Object constructors
#####################

## generic
setGeneric("D1Object", function(...) { standardGeneric("D1Object")} )

## no arguments in the signature
## setMethod("D1Object", , function() {
##   result <- new("D1Object")
##   return(result)
## })


setMethod("initialize", "D1Object", function(.Object, id, data, format, mnNodeId) {

  if (typeof(id) == "character") {
    message("@@ D1Object-class:R initialize as character")
    
    ## build identifier to be used in system metadata
    pid <- .jnew("org/dataone/service/types/v1/Identifier")
    pid$setValue(id)
    
    ## Convert incoming data to byte array (byte[])
    ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
    byteArray <- ioUtils$toByteArray(data)
    
    ## build the ObjectFormatIdentifier.
    formatId <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
    formatId$setValue(format)
    
    
    ## build a submitter Subject from the certificate
    certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
    cert <- certman$loadCertificate()
    submitter <- .jnew("org/dataone/service/types/v1/Subject")
    submitter$setValue(certman$getSubjectDN(cert))
    
    ## build the NodeReference
    mnNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
    mnNodeRef$setValue(mnNodeId)
    
    jd1o <- .jnew("org/dataone/client/D1Object",
                  pid, byteArray, formatId, submitter, mnNodeRef,
                  check=FALSE)
    
    if (!is.null(e <- .jgetEx())) {
      print("Java exception was raised")
      print(.jcheck(silent=TRUE))
      print(.jcheck(silent=TRUE))
      print(e)
      jd1o = .jnull("org/dataone/client/D1Object")
    }
  }
  else {
    message("@@ D1Object-class:R initialize as something else")
    if (.jinstanceof(id,"org/dataone/client/D1Object")) {
      message("@@ D1Object-class:R initialize with jobjRef")
      jd1o <- id
    }
  }
  
  .Object@jD1o <- jd1o
  return(.Object)
})

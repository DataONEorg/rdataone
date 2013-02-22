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

### This file contains functions useful to the dataone package methods


setClass("CertificateManager",
    representation(jClientIdManager = "jclassName")

)

setGeneric("CertificateManager", function(...) {
    standardGeneric("CertificateManager")
})


setGeneric("CertificateManager", function(...) {
            standardGeneric("CertificateManager")
        })


setMethod("CertificateManager", , function() {
   result <- new("CertificateManager")
   result@jClientIdManager <- J("org/dataone/client/auth/ClientIdentityManager")
   return(result)
})

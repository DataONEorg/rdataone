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

### This file contains the methods and accessors for D1Object objects

#########################################################
### MNRead and MNStorage methods
#########################################################

## createD1Object, implemented as a static way of constructing an object
# Consider making this into a real constructor
# Currently this is just a copy from the D1Client class, needs to be
# refactored
setGeneric("createD1Object", function(x, identifier, ...) { 
    standardGeneric("createD1Object")
})

setMethod("createD1Object", "D1Object", function(x, identifier, data, format, nodeId) {
   # Create identifier to be used in system metadata
   pid <- .jnew("org/dataone/service/types/v1/Identifier")
   pid$setValue(identifier)

   # Convert incoming data to byte array (byte[])
   ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
   byteArray <- ioUtils$toByteArray(data)

   # Create the ObjectFormatIdentifier.
   formatId <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
   formatId$setValue(format)

   # Set up/convert additional system metadata fields
   # get the submitter from the certificate
   certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
   cert <- certman$loadCertificate()
   submitter <- .jnew("org/dataone/service/types/v1/Subject")
   submitter$setValue(certman$getSubjectDN(cert))

   # Create the NodeReference
   mnNodeRef <- .jnew("org/dataone/service/types/v1/NodeReference")
   mnNodeRef$setValue(nodeId)

   # Now create the object with the sysmeta values
   d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, formatId, submitter, mnNodeRef, check=FALSE)
   if (!is.null(e<-.jgetEx())) {
       print("Java exception was raised")
       print(.jcheck(silent=TRUE))
       print(.jcheck(silent=TRUE))
       print(e)
   }

   return(d1object)
})

#########################################################
### Accessor methods
#########################################################

# Need accessors for sysmeta and data contained in the 
# internal java D1Object instance

#########################################################
### Utility methods
#########################################################


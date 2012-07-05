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
### MNRead ad MNStorage methods
#########################################################

## getD1Object, as a static way to get in instance of a D1Object
setGeneric("getD1Object", function(x, identifier, ...) { 
    standardGeneric("getD1Object")
})

setMethod("getD1Object", "D1Object", function(x, identifier) {

   result <- D1Object()

   pid <- .jnew("org/dataone/service/types/v1/Identifier")
   pid$setValue(identifier)

   # Use libclient D1Object to get contents of object
   d1obj <- J("org/dataone/client/D1Object")$download(pid)
   result@d1o <- d1obj

   #databytes <- d1obj$getData() 
   #.jcheck(silent = FALSE)

   #print("Convert to string...")
   #jString <-  .jnew("java/lang/String", databytes) 
   #.jcheck(silent = FALSE)
   #rdata <- jString$toString()

   #print("Pull out sysmeta...")
   #sysmeta <- d1obj$getSystemMetadata()
   #scimeta <- "Placeholder string for science metadata, waiting to implement lookup of sci metadata from describedBy field in sysmeta"
   #dp <- DataPackage(identifier, sysmeta, scimeta)
   #dp <- addData(dp, rdata)
   #return(dp)
   return(result)
})

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
   ioUtils <-  .jnew("org/apache/commons/io/IOUtils") 
   byteArray <- ioUtils$toByteArray(data)

   # Set up/convert additional system metadata fields
   # get the submitter from the certificate
   certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
   cert <- certman$loadCertificate()
   submitter <- certman$getSubjectDN(cert)

   # Now create the object with the sysmeta values
   d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, format, submitter, nodeId, check=FALSE)
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


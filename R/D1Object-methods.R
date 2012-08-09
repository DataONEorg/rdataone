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
setGeneric("createD1Object", function(id, data, format, d1Client, ...) { 
  standardGeneric("createD1Object")
})

setMethod("createD1Object",
          signature("character", "character", "character", "D1Client"),
	  function(id, data, format, d1Client) {

  # Create identifier to be used in system metadata
  pid <- .jnew("org/dataone/service/types/v1/Identifier")
  pid$setValue(id)

  # Set up/convert additional system metadata fields
  # get the submitter from the certificate
  certman <- J("org/dataone/client/auth/CertificateManager")$getInstance()
  cert <- certman$loadCertificate()
  submitter <- .jnew("org/dataone/service/types/v1/Subject")
  submitter$setValue(certman$getSubjectDN(cert))

  # Try and reserve this pid.
  print(paste("Reserving pid:", pid$getValue()))
  cnode <- getCN(d1Client)
  pid <- cnode$reserveIdentifier(pid)
  if (!is.null(e <- .jgetEx())) {
    print(paste("detail code:", e$getDetail_code()))
    if(e$getDetail_code() == "4210") {
       print(paste(identifier, "id cannot be used"))
    } else {
       print(e)
    }
    return
  }
  print(paste("Reserved pid:", pid$getValue()))

  # Convert incoming data to byte array (byte[])
  ioUtils <- .jnew("org/apache/commons/io/IOUtils") 
  byteArray <- ioUtils$toByteArray(data)

  # Create the ObjectFormatIdentifier.
  format.id <- .jnew("org/dataone/service/types/v1/ObjectFormatIdentifier")
  format.id$setValue(format)

  # Get the NodeReference from the mn node id.
  print("Getting MNodeId")
  node_id <- getMNodeId(d1Client)
  if(is.null(node_id) || (node_id == "")) {
    print("ERROR: A Member Node must be defined to create an object.")
    return(.jnull("org/dataone/client/D1Object"))
  }
  mn.noderef <- .jnew("org/dataone/service/types/v1/NodeReference")
  mn.noderef$setValue(node_id)
  print("got MN NodeReference")

  # Now create the object with the sysmeta values
print("creating object")
  d1object <- .jnew("org/dataone/client/D1Object", pid, byteArray, format.id,
                    submitter, mn.noderef, check=FALSE)
  if (!is.null(e <- .jgetEx())) {
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


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
library(stringr)

setClass("CertificateManager",
    representation(jClientIdManager = "jclassName", location="character", obscuredpath="character")
)

setGeneric("CertificateManager", function(...) {
    standardGeneric("CertificateManager")
})

setMethod("CertificateManager", , function() {
   result <- new("CertificateManager")
   result@location=as.character(NA)
   result@obscuredpath=as.character(NA)
   result@jClientIdManager <- J("org/dataone/client/auth/ClientIdentityManager")
   return(result)
})

## Get DataONE Identity as Stored in the CILogon Certificate
## 
## Returns Your Identity according to DataONE (and CILogon).  If the certificate
## is expired, the character string will be prefixed with "[EXPIRED]"
## @returnType character
## @return the DataONE Subject that is your client's identity
## 
## @author Matt Jones
## @export

setGeneric("showClientSubject", function(x, ...) { 
            standardGeneric("showClientSubject")
        })

setMethod("showClientSubject", signature("CertificateManager"), function(x) {
    
    PUBLIC="public"
    certfile <- getCertLocation(x)
    if (!is.null(certfile)) {
        cert <- PKI.load.cert(file=certfile)
        subject <- PKI.get.subject(cert)
    } else {
        subject=PUBLIC
    }
    ## since there's a certificate, now check to see if its expired, and if so, return PUBLIC
    if (isCertExpired(x)) {
        return(PUBLIC)
    }
    return(subject)
})

## Is the CILogon Certificate Expired?
## @returnType logical
## @return true if expired
## 
## @author Matt Jones
## @export


setGeneric("isCertExpired", function(x, ...) { 
            standardGeneric("isCertExpired")
        })

setMethod("isCertExpired", signature("CertificateManager"), function(x) {
    expires <- getCertExpires(x)
    if (is.null(expires)) {
        return(TRUE)
    } else {
        now <- Sys.time()
        if (expires < now) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }
})

## Show the Date and Time when the CILogon Certificate Expires
## @returnType character
## @return the expiration date
## 
## @author Matt Jones
## @export

setGeneric("getCertExpires", function(x, ...) { 
            standardGeneric("getCertExpires")
        })

setMethod("getCertExpires", signature("CertificateManager"), function(x) {
    certfile <- getCertLocation(x)
    if (!is.null(certfile)) {
        cert <- PKI.load.cert(file=certfile)
        expires <- PKI.get.notAfter(cert)
    } else {
        expires=NULL
    }
    return(expires)
})


## open the CILogin Certificate download page in the default browser
## 
## A convenience method to take you to the CILogon download page:  
## https://cilogon.org/?skin=DataONE
## @returnType NULL
## 
## @author rnahf
## @export
setGeneric("downloadCert", function(x, ...) { 
            standardGeneric("downloadCert")
        })

setMethod("downloadCert", signature("CertificateManager"), function(x) {
    browseURL("https://cilogon.org/?skin=DataONE")
})



## Obscure the CILogon Client Certificate
## 
## Obscures the x509 certificate that CILogon installs, effectively making future
## interactions with the DataONE services anonymous.  Note, when the client
## certificate is obscured, you will not be able to create objects to DataONE,
## or build D1Objects, which uses the certificate to fill out fields in the
## system metadata.
## 
## @note \code{restoreCert} is this method's inverse operation   
## @returnType NULL
## 
## @author Matt Jones
## @export
setGeneric("obscureCert", function(x, ...) { 
            standardGeneric("obscureCert")
        })

setMethod("obscureCert", signature("CertificateManager"), function(x) {
    certpath <- getCertLocation(x)
    if (!is.null(certpath)) {
        x@obscuredpath <- paste0(certpath, "_obscured")
        file.rename(certpath, x@obscuredpath)
    }
    return(x)
})


## Restore an Obscured Certificate
## 
## Restores an obscured certificate to its original location. The inverse
## operation to "obscureCert".  
## 
## @returnType NULL
## 
## @author Matt Jones
## @export
setGeneric("restoreCert", function(x, ...) { 
            standardGeneric("restoreCert")
        })

setMethod("restoreCert", signature("CertificateManager"), function(x) {

    certpath <- getCertLocation(x)
    if (!is.null(certpath)) {
        ## if we got here, a new certificate is in the default location, so
        ## remove any obscured certificate
        if (!is.na(x@obscuredpath)) {
            file.remove(x@obscuredpath)
            x@obscuredpath = as.character(NA)
        }
    } else { 
        expectedLoc <- str_sub(x@obscuredpath, end=(str_locate(x@obscuredpath, "_obscured")[[1]]-1))
        # message("expected:",expectedLoc," obscured: ", x@obscuredpath)
        if (file.exists(x@obscuredpath)) {
            file.rename(x@obscuredpath, expectedLoc)
        } else {
            message("No obscured certificate to restore at", x@obscuredpath)
        }
        x@obscuredpath <- as.character(NA)
    }
})

## Get the location on disk of the client certificate file
## 
## @returnType character
## 
## @author Matt Jones
## @export
setGeneric("getCertLocation", function(x, ...) { 
    standardGeneric("getCertLocation")
})

setMethod("getCertLocation", signature("CertificateManager"), function(x) {
    
    # default Globus Grid Security Infrastructure (GSI) location, which is /tmp/x509up_u${UID} on Unix 
    # or ${tmpdir}/x509up_u${UID} on Windows or ${tmpdir}/x509up_u${user.name} if ${UID} is not defined
    
    # If a custom location is set, then just return that
    if (!is.na(x@location)) {
        print(paste("Using custom location!"))
        return(x@location)
    }
    
    # Temp directory locations to check
    loclist <- list(c('/tmp', Sys.getenv('TMPDIR', names=FALSE), Sys.getenv('TEMP', names=FALSE)))
    
    # Find the user's UID
    uid <- as.numeric(system('id -u', intern=TRUE)) # TODO: this only works on *nix, not Windows!
        
    # If UID is null or not a number, try the username
    if (is.null(uid)) {
        uid <- Sys.info()['user']
    }
    
    # Our default is to return NULL if a cert file is not found
    location=NULL
    
    counter = 0
    # Check each file path in order to see if the cert file exists, and if so, return it
    for(dir in loclist) {
        counter = counter+1
        # Construct the default path to the filename
        certpath <- paste(dir[[counter]], "/x509up_u", uid, sep="")
        # Check if the file exists
        if (file.exists(certpath)) {
            location=certpath
            break
        }
    }
    
    # No file exists in the default locations, so return NULL
    return(location)
})

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


## Get DataONE Identity as Stored in the CILogon Certificate
## 
## Returns Your Identity according to DataONE (and CILogon).  If the certificate
## is expired, the character string will be prefixed with "[EXPIRED]"
## @returnType character
## @return the DataONE Subject that is your client's identity
## 
## @author rnahf
## @export

setGeneric("showClientSubject", function(x, ...) { 
            standardGeneric("showClientSubject")
        })

setMethod("showClientSubject", signature("CertificateManager"), function(x) {
    
    jSubject <- x@jClientIdManager$getCurrentIdentity()
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    subjectValue <- jSubject$getValue()
    if (subjectValue == J("org/dataone/service/util/Constants")$SUBJECT_PUBLIC) {
        return(subjectValue)
    }
    
    ## since there's a certificate, now check to see if its expired
    if (isCertExpired(x)) {
        return(paste("[EXPIRED]", jSubject$getValue()))
    }
    return(jSubject$getValue())
})




## Is the CILogon Certificate Expired?
## @returnType logical
## @return true if expired
## 
## @author rnahf
## @export


setGeneric("isCertExpired", function(x, ...) { 
            standardGeneric("isCertExpired")
        })

setMethod("isCertExpired", signature("CertificateManager"), function(x) {
    ## since there's a certificate, now check to see if its expired
    jExpDate <- x@jClientIdManager$getCertificateExpiration()
    if (!is.null(jExpDate)) {
        jNowDate <- .jnew("java/util/Date")
        if (jExpDate$before(jNowDate)) {
            return(TRUE)
        }
    }
    return(FALSE)
})




## Show the Date and Time when the CILogon Certificate Expires
## @returnType character
## @return the expiration date
## 
## @author rnahf
## @export

setGeneric("getCertExpires", function(x, ...) { 
            standardGeneric("getCertExpires")
        })

setMethod("getCertExpires", signature("CertificateManager"), function(x) {
    jDate <- x@jClientIdManager$getCertificateExpiration()
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    if (is.null(jDate)) {
        return(NULL)
    }
    return(jDate$toString())
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
## @author rnahf
## @export
setGeneric("obscureCert", function(x, ...) { 
            standardGeneric("obscureCert")
        })

setMethod("obscureCert", signature("CertificateManager"), function(x) {
    jFile <- J("org/dataone/client/auth/CertificateManager")$getInstance()$locateDefaultCertificate()
    # check for FileNotFound
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    filePath <- jFile$getAbsolutePath()
    file.rename(filePath,paste0(filePath,"_obscured"))
})




## Restore an Obscured Certificate
## 
## Restores an obscured certificate to its original location. The inverse
## operation to "obscureCert".  
## 
## @returnType NULL
## 
## @author rnahf
## @export
setGeneric("restoreCert", function(x, ...) { 
            standardGeneric("restoreCert")
        })

setMethod("restoreCert", signature("CertificateManager"), function(x) {

    # check for FileNotFound
    tryCatch({
        jFile <- J("org/dataone/client/auth/CertificateManager")$getInstance()$locateDefaultCertificate()
        ## if we got here, a new certificate is in the default location, so
        ## remove any obscured certificate
        file.remove(paste0(jFile$getAbsolutePath,"_obscured"))
    }, error=function(err) { 
        expectedLoc <- sub("(.+expected location: )","",err$getMessage())
        obscured <- paste0(expectedLoc,"_obscured")
        message("expected:",expectedLoc," obscured: ", obscured)
        if (file.exists(obscured)) {
            file.rename(obscured, expectedLoc)
        } else {
            message("No obscured certificate to restore at", obscured)
        }
    })
})



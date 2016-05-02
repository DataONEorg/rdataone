#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2012-2014
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

#' CertficateManager provides mechanisms to obtain, load, verify, and display X509 certificates.
#' @description CertficateManager provides management functions for X.509 certificates that are 
#' used to authenticate connections to DataONE nodes over SSL.  The X.509 certificates are issued 
#' by a recognized Certificate Authority, typically CILogon, and include fields that provide 
#' information about the authenticated party, including the distinguished name of the subject, the 
#' dates of validity of the certificate, and other information needed for authorization decisions.  
#' Certificate validity is determined by examining the validity of the certificate signatures for 
#' each certificate in a chain leading to a trusted root certificate.  Within DataONE, the current 
#' trusted root certificate authorities are CILogon and DataONE itself.
#' 
#' @details   
#' Understanding how your identity is managed is important for working with DataONE, especially to 
#' avoid unexpected results. For example, depending your authorization status, searches may or may 
#' return only public records, or the full set of public and private records. Object and package 
#' retrievals might fail if some or all of the objects being retrieved are private.  Creating or 
#' updating objects on DataONE nodes and reserving identifiers reservations might fail if your 
#' authorization certificate is missing or expired.
#' 
#' DataONE identifies you using CILogon-provided x509 certificates. DataONE has 
#' partnered with CILogon to provide a widely-accessible certificate issuing mechanism 
#' that allows DataONE users to use existing trusted institutional and public accounts.
#' 
#' CILogon recognizes many identity providers, including many universities as well as
#' Google, so most times users new to DataONE can get certificates using one
#' of their existing accounts. For more information about the CILogon service, see 
#' "https://cilogon.org/?skin=DataONE" .
#' 
#' X509 Certificates differ from typical username-password login schemes in that
#' certificates can be used by more than one application, which is very useful when
#' using more than one DataONE-enabled application.  The certificates CILogon issues
#' for DataONE are so-called "short-lived" certificates that currently expire 18 hours 
#' from the time of issuing.  Typically you will want to download a fresh certificate
#' the first time you interact with DataONE each day.
#' 
#' @slot location value of type \code{"character"}, containing a path to a custom certificate location
#' @slot obscuredpath value of type \code{"character"}, containing the path used to temporarily obscure a certificate
#' @author Matthew Jones, Rob Nahf
#' @rdname CertificateManager-class
#' @keywords classes
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{CertificateManager}}}{: Create a CertificateManager object.}
#'  \item{\code{\link{getCertLocation}}}{: Get the file path on disk of the client certificate file.}
#'  \item{\code{\link{showClientSubject}}}{: Get DataONE Identity as Stored in the CILogon Certificate.}
#'  \item{\code{\link{isCertExpired}}}{: Determine if an X.509 certificate has expired.}
#'  \item{\code{\link{getCertExpires}}}{: Show the date and time when an X.509 certificate expires.}
#'  \item{\code{\link{downloadCert}}}{: Open the CILogon Certificate download page in the default browser.}
#'  \item{\code{\link{obscureCert}}}{: Obscure the CILogon Client Certificate.}
#'  \item{\code{\link{restoreCert}}}{: Restore the CILogon client certificate by renaming it to its original location}
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @examples
#' \dontrun{
#' cm <- suppressWarnings(CertificateManager())
#' cert <- getCertLocation(cm)
#' subject <- showClientSubject(cm)
#' expires <- getCertExpires(cm)
#' isExpired <- isCertExpired(cm)
#' cm <- obscureCert(cm)
#' cm <- restoreCert(cm)
#' }
setClass("CertificateManager", slots = c(
    location="character", 
    obscuredpath="character"
    )
)

#' Create a CertificateManager object
#' @description Construct an instance of CertficateManager to provide mechanisms to obtain, load, verify, and 
#' display X509 certificates.  If the \code{'location'} field is provided, then that location is interpreted
#' as the fully qualified path to a certificate on the local filesystem, and the default locations will not be
#' searched.  If \code{'location'} is missing, then the default Globus Grid Security Infrastructure (GSI) 
#' location is searched, which is \code{'/tmp/x509up_u${UID}'} on Unix 
#' or \code{'${tmpdir}/x509up_u${UID}'} on Windows or \code{'${tmpdir}/x509up_u${user.name}'} if \code{'${UID}'} 
#' is not defined.
#' @param ... (Not yet used)
#' @return the CertificateManager object
#' @export
setGeneric("CertificateManager", function(...) {
    .Deprecated("AuthenticationManager", "dataone")
    standardGeneric("CertificateManager") 
})

#' @rdname CertificateManager
setMethod("CertificateManager", signature=character(), function() {
    if (!requireNamespace("openssl", quietly = TRUE)) {
        stop("CertificateManager functions require the openssl package to be installed.")
    }
    result <- new("CertificateManager")
    result@location=as.character(NA)
    result@obscuredpath=as.character(NA)
    return(result)
})

#' Get DataONE Identity as Stored in the CILogon Certificate.
#' @description Returns Your Identity according to DataONE (and CILogon) as provided in the Subject
#' field of the X.509 certificate.  The value is a Distinguished Name, and can be used in all fields that
#' require a user identity for access control authorization. If the certificate is missing on expired, then
#' the subject 'public' is returned.
#' @rdname showClientSubject
#' @aliases showClientSubject
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @return the DataONE Subject that is your client's identity
#' @export
setGeneric("showClientSubject", function(x, ...) { 
    .Deprecated("getCertInfo", "dataone")
    standardGeneric("showClientSubject")
})

#' @rdname showClientSubject
setMethod("showClientSubject", signature("CertificateManager"), function(x) {
    PUBLIC="public"
    certfile <- getCertLocation(x)
    if (!is.null(certfile)) {
        cert <- openssl::read_cert(file=certfile)
        subject <- as.list(cert)$subject
    } else {
        subject=PUBLIC
    }
    ## since there's a certificate, now check to see if its expired, and if so, return PUBLIC
    if (isCertExpired(x)) {
        return(PUBLIC)
    }
    return(subject)
})

#' Determine if an X.509 certificate has expired.
#' @description Returns \code{'TRUE'} if the certificate associated with a CertificateManager instance is expired.
#' A certificate is expired if any of the following conditions hold: 1) the current time is before or after the 
#' certificate validity dates, 2) the certificate is not valid according to a trusted Certificate Authority, or
#' 3) no certificate can be found.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname isCertExpired
#' @aliases isCertExpired
#' @return TRUE if the certificate is expired
#' @export
setGeneric("isCertExpired", function(x, ...) { 
    .Deprecated("getCertInfo", "dataone")
            standardGeneric("isCertExpired")
        })

#' @rdname isCertExpired
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

#' Show the date and time when an X.509 certificate expires.
#' @description Each X.509 has a range of certificate validity times.  This method returns the X.509 
#' \code{'notAfter'} field formatted as a \code{'POSIXct'} date value.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname getCertExpires
#' @aliases getCertExpires
#' @return POSIXct value
#' @export
setGeneric("getCertExpires", function(x, ...) { 
    .Deprecated("getCertInfo", "dataone")
            standardGeneric("getCertExpires")
        })

#' @rdname getCertExpires
setMethod("getCertExpires", signature("CertificateManager"), function(x) {
    certfile <- getCertLocation(x)
    if (!is.null(certfile)) {
        cert <- openssl::read_cert(file=certfile)
        expires <- as.POSIXct(as.list(cert)$validity[[2]], format="%b %d %H:%M:%S %Y", tz="GMT")
    } else {
        expires=NULL
    }
    return(expires)
})

#' Open the CILogon Certificate download page in the default browser.
#' @description A convenience method to take you to the CILogon download page:  
#' "https://cilogon.org/?skin=DataONE. Logging into CILogon will allow
#' you to download your X.509 certificate to your local computer.  Typically,
#' the certificate is saved in the default Globus location for certificates 
#' (\code{\link{getCertLocation}}) and once it is there, the \code{'dataone'} 
#' package will use the certificate for all authenticated operations.  Deleting 
#' the certificate file is the equivalent of logging out.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname downloadCert
#' @aliases downloadCert
#' @return NULL
#' @export
#' @importFrom utils browseURL
setGeneric("downloadCert", function(x, ...) { 
    standardGeneric("downloadCert")
})

#' @rdname downloadCert
setMethod("downloadCert", signature("CertificateManager"), function(x) {
    browseURL("https://cilogon.org/?skin=DataONE")
})

#' Obscure the CILogon Client Certificate
#' @description Obscures the x509 certificate that CILogon installs, effectively making future
#' interactions with the DataONE services public/anonymous.  This function simple renames an
#' existing certificate file to a known location, allowing 'public' operations.  Note, when the 
#' client certificate is obscured via the renaming, you will not be able to create objects in 
#' DataONE, or utilize any other methods that require authentication.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname obscureCert
#' @aliases obscureCert
#' @return the modified CertificateManager instance
#' @seealso \code{\link{restoreCert}} is this method's inverse operation   
#' @export
setGeneric("obscureCert", function(x, ...) { 
    .Deprecated("obscureAuth", "dataone")
    standardGeneric("obscureCert")
})

#' @rdname obscureCert
setMethod("obscureCert", signature("CertificateManager"), function(x) {
    certpath <- getCertLocation(x)
    if (!is.null(certpath)) {
        x@obscuredpath <- paste0(certpath, "_obscured")
        file.rename(certpath, x@obscuredpath)
    }
    return(x)
})

#' Restore the CILogon client certificate by renaming it to its original location
#' @description Restores the x509 certificate that CILogon installs, which allows future
#' interactions with nodes to be authenticated with the certificate.  This function simply 
#' renames an obscured certificate file to its original location, allowing authenticated 
#' operations.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname restoreCert
#' @aliases restoreCert
#' @return the modified CertificateManager instance
#' @seealso \code{\link{obscureCert}} is this method's inverse operation
#' @import stringr
#' @export
setGeneric("restoreCert", function(x, ...) { 
    .Deprecated("restoreAuth", "dataone")
    standardGeneric("restoreCert")
})

#' @rdname restoreCert
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
            message("No obscured certificate to restore at ", x@obscuredpath)
        }
        x@obscuredpath <- as.character(NA)
    }
    return(x)
})

#' Get the file path on disk of the client certificate file.
#' @description Find the location of the client certificate, which is typically in a default
#' location on disk, unless the \code{'location'} slot has been set with a custom location for
#' the certificate.
#' @details The default  Globus Grid Security Infrastructure (GSI) location is \code{'/tmp/x509up_u${UID}'} 
#' on Unix or \code{'${tmpdir}/x509up_u${UID}'} on Windows or \code{'${tmpdir}/x509up_u${user.name}'} 
#' if \code{'${UID}'} is not defined.
#' @param x a CertificateManager instance
#' @param ... (Not yet used)
#' @rdname getCertLocation
#' @aliases getCertLocation
#' @return character the path to the certificate
#' @export
setGeneric("getCertLocation", function(x, ...) { 
    .Deprecated("getCertInfo", "dataone")
    standardGeneric("getCertLocation")
})

#' @rdname getCertLocation
setMethod("getCertLocation", signature("CertificateManager"), function(x) {
    # default Globus Grid Security Infrastructure (GSI) location, which is /tmp/x509up_u${UID} on Unix 
    # or ${tmpdir}/x509up_u${UID} on Windows or ${tmpdir}/x509up_u${user.name} if ${UID} is not defined
    
    # If a custom location is set, then just return that
    if (!is.na(x@location)) {
        return(x@location)
    }
    
    # Temp directory locations to check
    loclist <- list('/tmp', Sys.getenv('TMPDIR', names=FALSE), Sys.getenv('TEMP', names=FALSE))
    
    # Find the user's UID 
    
    # On windows, construct the same certificate filename as the GridShib-CA Logon Client 
    # that created/downloaded the certificate.
    if(Sys.info()[['sysname']] == "Windows") {
      uid <- Sys.info()['user']
      certFnBase <- "\\x509up_u_"
    } else {
      uid <- as.numeric(system('id -u', intern=TRUE))
      certFnBase <- "/x509up_u"
    }
    
    # If UID is null or not a number, try the username
    if (is.null(uid)) {
      uid <- Sys.info()['user']
    }
    
    # Our default is to return NULL if a cert file is not found
    location <- NULL
    counter <- 0
    # Check each file path in order to see if the cert file exists, and if so, return it
    for(counter in 1:length(loclist)) {
        # Construct the default path to the filename
        certpath <- paste(loclist[[counter]], certFnBase, uid, sep="")
        # Check if the file exists
        if (file.exists(certpath)) {
            location <- certpath
            break
        }
    }
    
    # No file exists in the default locations, so return NULL
    return(location)
})

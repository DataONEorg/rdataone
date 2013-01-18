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

### This file contains functions useful to the dataone package methods


convert.csv <-function(df, ...) {
    con <- textConnection("data", "w")
    write.csv(df, file=con, row.names = FALSE, col.names = TRUE, ...)
    close(con)
    csvdata <- paste(data, collapse="\n")
    return(csvdata)
}



# encodeSolr <- function(querySegment) {
#     backslashed <- gsub("([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","\\\\\\1",querySegment, perl=TRUE)
#     return(backslashed)
# }

encodeSolr <- function(segment) {
    inter <- gsub("([-+:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","\\\\\\1",segment, perl=TRUE) 
    if (grepl(" ",inter)) {
        return(paste0("\"",inter,"\""))
    }
    return(inter)
}

#' encodes the reserved characters in a url query segment
encodeUrlQuery <- function(querySegment) {
    
    #    luceneExample <- "+pool +ABQ\\:Bernalillo \\[NM\\] -sharks \"kids & adults = fun day\"" 
    #    luceneReservedCharExample <- "example__\\+_\\-_\\&_\\|_\\!_\\^_\\~_\\*_\\?_\\:_\\\"_\\(_\\)_\\{_\\}_\\[_\\]____"
    
    encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlQuerySegment", querySegment)
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    return(encoded)
    
    ## an R-only alternate implementation that only would work for ASCII characters
    ## (may need to check the behavior of {,},[,] - they may need to be hidden also)
    #    escaped <- gsub("\\\\([+-:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","%5C\\1",querySegment, perl=TRUE)
    #    escaped <- gsub("%5C&","%5C%26",solrQuery)  ##  need to hide the ampersand from the web server
    #    escaped   <- gsub("%5C\\+","%5C%2B",solrQuery)  ## need to hide the + from the web server
}

encodeUrlPath <- function(pathSegment) {
    encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlPathSegment", pathSegment)
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    return(encoded)
}


## showCurrentIdentity
# setGeneric("showCurrentIdentity", function(x) { 
#     standardGeneric("showCurrentIdentity")
# })
# 
# setMethod("showCurrentIdentity", signature("D1Client"), function(x) {

d1.getIdentity <- function() {
    
    jSubject <- J("org/dataone/client/auth/ClientIdentityManager")$getCurrentIdentity()
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    subjectValue <- jSubject$getValue()
    if (subjectValue == J("org/dataone/service/util/Constants")$SUBJECT_PUBLIC) {
        return(subjectValue)
    }
    
    ## since there's a certificate, now check to see if its expired
    if (d1.isCertExpired()) {
        return(paste("[EXPIRED]", jSubject$getValue()))
    }
    return(jSubject$getValue())
}


d1.isCertExpired <- function() {
    ## since there's a certificate, now check to see if its expired
    jExpDate <- J("org/dataone/client/auth/ClientIdentityManager")$getCertificateExpiration()
    if (!is.null(jExpDate)) {
        jNowDate <- .jnew("java/util/Date")
        if (jExpDate$before(jNowDate)) {
            return(TRUE)
        }
    }
    return(FALSE)
}


##
# setGeneric("showCertificateExpiration", function(x) {
#     standardGeneric("showCertificateExpiration")
# })
# 
# setMethod("showCertificateExpiration", signature("D1Client"), function(x) {

d1.getCertExpires <- function() {
    jDate <- J("org/dataone/client/auth/ClientIdentityManager")$getCertificateExpiration()
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    if (is.null(jDate)) {
        return(NULL)
    }
    return(jDate$toString())
}


#' open the CILogin Certificate download page in the default browser
# setGeneric("launchCertificateDownload", function(x) {
#     standardGeneric("launchCertificateDownload")
# })
# 
# setMethod("launchCertificateDownload", signature("D1Client"), function(x) {
d1.downloadCert <- function() {
    browseURL("https://cilogon.org/?skin=DataONE")
}

#' Obscures the client certificate CILogon installs, effectively making future
#' interactions with the DataONE services anonymous.  Note, when the client
#' certificate is obscured, you will not be able to create objects to DataONE,
#' or build D1Objects, which uses the certificate to fill out fields in the
#' system metadata.
#' restoreCert is this method's inverse operation   
# setGeneric("obscureCert", function(x) {
#     standardGeneric("obscureCert")
# })
# 
# setMethod("obscureCert", signature("D1Client"), function(x) {

d1.obscureCert <- function() {
    jFile <- J("org/dataone/client/auth/CertificateManager")$getInstance()$locateDefaultCertificate()
    # check for FileNotFound
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    filePath <- jFile$getAbsolutePath()
    file.rename(filePath,paste0(filePath,"_obscured"))
}

#' Restores an obscured certificate to its original location.  The inverse
#' operation to "obscureCert".  
# setGeneric("restoreCert", function(x) {
#     standardGeneric("restoreCert")
# })
# 
# setMethod("restoreCert", signature("D1Client"), function(x) {
   
d1.restoreCert <- function() {
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
}




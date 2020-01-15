#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2012-2014
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except i mplecompliance with the License.
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

#' @title Manage DataONE authentication.
#' @description AuthenticationManager provides mechanisms to validate DataONE authentication,
#' when either a DataONE authentication token or X.509 Certificate is used.
#' @details   
#' Understanding how your identity is managed is important for working with DataONE, especially to 
#' avoid unexpected results. For example, depending your authorization status, searches may
#' return only public records, or the full set of public and private records. Object and package 
#' retrievals might fail if some or all of the objects being retrieved are private.  Creating or 
#' updating objects on DataONE nodes and reserving identifiers might fail if your 
#' authorization credentials are missing or expired.
#' 
#' DataONE version 1.0 identifies you using CILogon-provided x509 certificates. DataONE has 
#' partnered with CILogon to provide a widely-accessible certificate issuing mechanism 
#' that allows DataONE users to use existing trusted institutional and public accounts.
#' 
#' DataONE version 2.0 provides an addition authentication mechanism known as
#' authentication tokens. For information about tokens and instructions for generating
#' a token for use with the dataone R package, view the overview document by
#' entering the command: \code{'vignette("dataone-overview")'}. DataONE authentication
#' tokens can be obtained by signing in to your DataONE account at https://search.dataone.org.
#' 
#' CILogon recognizes many identity providers, including many universities as well as
#' Google, so most times users new to DataONE can get certificates using one
#' of their existing accounts. For more information about the CILogon service, see 
#' \url{https://cilogon.org/?skin=DataONE} .
#' @slot obscured Value of type \code{"character"} Is authentication disabled (obscured)?
#' @rdname AuthenticationManager-class
#' @aliases AuthenticationManager-class
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{AuthenticationManager}}}{: Create an AuthenticationManager object.}
#'  \item{\code{\link{isAuthValid}}}{: Verify authentication for a member node.}
#'  \item{\code{\link{getToken}}}{: Get the value of the DataONE Authentication Token, if one exists.}
#'  \item{\code{\link{getCert}}}{: Get the DataONE X.509 Certificate location.}
#'  \item{\code{\link{getAuthMethod}}}{: Get the current valid authentication mechanism.}
#'  \item{\code{\link{getAuthSubject}}}{: Get the authentication subject.}
#'  \item{\code{\link{getAuthExpires}}}{: Get the expiration date of the current authentication method.}
#'  \item{\code{\link{isAuthExpired}}}{: Check if the currently valid authentication method has reached the expiration time.}
#'  \item{\code{\link{obscureAuth}}}{: Temporarily disable DataONE authentication.}
#'  \item{\code{\link{restoreAuth}}}{: Restore authentication (after being disabled with \code{obscureAuth}).}
#'  \item{\code{\link{showAuth}}}{: Display all authentication information.}
#'  \item{\code{\link{getTokenInfo}}}{: Display all authentication token information.}
#'  \item{\code{\link{getCertInfo}}}{: Display all X.509 certificate information.}
#'  
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @import hash
#' @import base64enc
#' @importFrom jsonlite fromJSON
#' @include D1Node.R
#' @export
setClass("AuthenticationManager", slots = c(
    obscured="logical"
    )
)

#' Create an AuthenticationManager object
#' @description Construct an instance of AuthenticationManager to provide mechanisms to load, verify, and 
#' display DataONE authentication information.  
#' @param ... (Not yet used)
#' @return the AuthenticationManager object
#' @export
setGeneric("AuthenticationManager", function(...) {
    standardGeneric("AuthenticationManager")
})

#' @rdname AuthenticationManager
#' @export
setMethod("AuthenticationManager", signature=character(), function() {
    .Object <- new("AuthenticationManager")
    .Object@obscured <- FALSE
    return(.Object)
})

#' Verify authentication for a member node.
#' @rdname isAuthValid
#' @aliases isAuthValid
#' @description The currently used DataONE client authentication method (either tokens or X.509 certificates)
#' is checked and verified for the specified node (either CN or MN). If an authentication token is available
#' via the R options facility, it will be used i.e. available via getOption("dataone_token").  However, 
#' authentication tokens can only be used for DataONE v2 or higher nodes. X.509 certificates can be used 
#' with DataONE v1 or higher nodes. 
#' See the \emph{"dataone"} vignette \emph{"dataone-overview"} for more information on authentication.
#' @param .Object An AuthenticationManager instance
#' @param node The node object (MNode or CNode) that authentication is being checked for.
#' @param ... additional parameters
#' @return A logical value: TRUE if authentication is valid, false if not.
setGeneric("isAuthValid", function(.Object, ...) { 
    standardGeneric("isAuthValid")
})

#' @rdname isAuthValid
setMethod("isAuthValid", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- evaluateAuth(.Object, node)
    return(authInfo$valid)
})

#' Get the value of the DataONE Authentication Token, if one exists.
#' @details A token value is retrieved based on the DataONE environment that the specified node is
#' located in, either the production environment or a test environment.
#' @rdname getToken
#' @aliases getToken
#' @param .Object an AuthenticationManager instance
#' @param ... additional parameters
#' @return The current authentication token.
setGeneric("getToken", function(.Object, ...) { 
    standardGeneric("getToken")
})

#' @rdname getToken
#' @param node either a CNode or MNode object to get the appropriate token for.
setMethod("getToken", signature("AuthenticationManager"), function(.Object, node=as.character(NA)) {
    if(.Object@obscured) {
        return(as.character(NA))
    } else {
        stopifnot(is(node, "CNode") || is(node, "MNode"))
        if (node@env == "test") {
            token <- getOption("dataone_test_token")
            if(!is.null(token) && !is.na(token)) {
                attr(token, "name") <- "dataone_test_token"
            }
            return(token)
        } else {
            # Look for a production token
            token <- getOption("dataone_token")
            if(!is.null(token) && !is.na(token)) {
                attr(token, "name") <- "dataone_token"
            }
            return(token)
        }
    }
})
    
#' Get the DataONE X.509 Certificate location.
#' @rdname getCert
#' @aliases getCert
#' @param .Object an AuthenticationManager instance
#' @param ... (Not yet used)
#' @return The filename of the current DataONE X.509 Certificate if it is available.
setGeneric("getCert", function(.Object, ...) { 
    standardGeneric("getCert")
})

#' @rdname getCert
setMethod("getCert", signature("AuthenticationManager"), function(.Object) { 
    if(.Object@obscured) {
        return(as.character(NA))
    } else {
        suppressWarnings(cm <- CertificateManager())
        return(suppressWarnings(getCertLocation(cm)))
    }
})

#' Get the current valid authentication mechanism.
#' @rdname getAuthMethod
#' @aliases getAuthMethod
#' @param .Object An AuthenticationManager instance
#' @param ... (Not yet used)
#' @return The current authentication mechanism as a character string, either "token" or "cert".
setGeneric("getAuthMethod", function(.Object, ...) { 
    standardGeneric("getAuthMethod")
})

#' @rdname getAuthMethod
#' @details The current authentication method being used, either an authentication token or an X.509 certificate. The \code{'node'}
#' argument is used to determine the authentication mechanism that is appropriate for the specified \code{'node'}.
#' For example, authentication tokens are supported on DataONE nodes that use the DataONE V2.0 API or higher, so if the 
#' node uses the V1 API, then only an X.509 certificate can be used.
#' @param node A D1Node instance to determine the authentication method for.
setMethod("getAuthMethod", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- evaluateAuth(.Object, node)
    return(authInfo$authMethod)
})

#' Get the authentication subject.
#' @rdname getAuthSubject
#' @aliases getAuthSubject
#' @param .Object an AuthenticationManager instance
#' @param ... (Not yet used)
#' @return the DataONE Subject that is your client's identity
setGeneric("getAuthSubject", function(.Object, ...) { 
    standardGeneric("getAuthSubject")
})

#' @rdname getAuthSubject
#' @details The authenticated user, aka 'subject' is retrieved from the authentication mechanism
#' currently being used, either an authentication token or an X.509 certificate. The \code{'node'}
#' argument is used to determine the authentication mechanism that is appropriate for the specified \code{'node'}.
#' For example, authentication tokens are supported on DataONE nodes that use the DataONE V2.0 API or higher, so if the 
#' node uses the V1 API, then only an X.509 certificate can be used.
#' @param node A D1Node instance
setMethod("getAuthSubject", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- evaluateAuth(.Object, node)
    return(authInfo$subject)
})

#' Get the expiration date of the current authentication method.
#' @description The expiration date of the current authentication method, either
#' authentication token or X.509 certificate, is returned as a Greenwich Mean Time (GMT) value.
#' @rdname getAuthExpires
#' @aliases getAuthExpires
#' @param .Object An AuthenticationManager instance
#' @param node A D1Node instance
#' @return The expiration date for the current authentication mechanism being used.
setGeneric("getAuthExpires", function(.Object, node) { 
    standardGeneric("getAuthExpires")
})

#' @rdname getAuthExpires
setMethod("getAuthExpires", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- evaluateAuth(.Object, node)
    return(authInfo$expires)
})
#' Check if the currently valid authentication method has reached the expiration time.
#' @rdname isAuthExpired
#' @aliases isAuthExpired
#' @param .Object An AuthenticationManager instance
#' @param node A D1Node instance
#' @param ... (Not yet used)
#' @return A logical value: TRUE if authentication has expired, FALSE if not.
setGeneric("isAuthExpired", function(.Object, ...) { 
    standardGeneric("isAuthExpired")
})

#' @rdname isAuthExpired
setMethod("isAuthExpired", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- evaluateAuth(.Object, node)
    return(authInfo$expired)
})

#' Temporarily disable DataONE authentication.
#' @description Calling \code{obscureAuth} temporarily disables authentication so that
#' @details This method is intended to be used for authentication testing.
#' \code{isAuthValid} will return FALSE. Authentication can be re-enabled by calling
#' \code{restoreAuth}.
#' @rdname obscureAuth
#' @aliases obscureAuth
#' @param .Object An AuthenticationManager instance
#' @return The expiration date for the current authentication mechanism being used.
setGeneric("obscureAuth", function(.Object) { 
    standardGeneric("obscureAuth")
})

#' @rdname obscureAuth
setMethod("obscureAuth", signature("AuthenticationManager"), function(.Object) {
    .Object@obscured <- TRUE
    return(.Object)
})

#' Restore authentication (after being disabled with \code{obscureAuth}).
#' @rdname restoreAuth
#' @aliases restoreAuth
#' @param .Object An AuthenticationManager instance
#' @return The expiration date for the current authentication mechanism being used.
setGeneric("restoreAuth", function(.Object) { 
    standardGeneric("restoreAuth")
})

#' @rdname restoreAuth
setMethod("restoreAuth", signature("AuthenticationManager"), function(.Object) {
    .Object@obscured <- FALSE
    return(.Object)
})

#' Display all authentication information
#' @rdname showAuth
#' @aliases showAuth
#' @param .Object An AuthenticationManager instance
#' @param node A D1Node instance
#' @param ... (Not yet used)
setGeneric("showAuth", function(.Object, ...) { 
    standardGeneric("showAuth")
})

#' @rdname showAuth
setMethod("showAuth", signature("AuthenticationManager"), function(.Object, node) {
    suppressMessages(authInfo <- evaluateAuth(.Object, node))
    message(sprintf("authentication method: %s", authInfo$authMethod))
    message(sprintf("token: %s", authInfo$token))
    message(sprintf("cert: %s", authInfo$cert))
    message(sprintf("node: %s", authInfo$node))
    message(sprintf("valid: %s", as.character(authInfo$valid)))
    message(sprintf("expires: %s", as.character(authInfo$expires)))
    message(sprintf("subject: %s" , authInfo$subject))
    invisible(authInfo)
})

parseAuthToken <- function(authToken) {
    # TODO: put in checks for a valid token format
    payload <- unlist(strsplit(authToken, "\\."))[[2]]
    payloadRaw <- base64decode(payload)
    payloadChar <- rawToChar(payloadRaw)
    tokenInfo <- fromJSON(payloadChar)
    return(tokenInfo)
}
#' Evaluate DataONE authentication.
#' @description A valid DataONE authentication method is looked for and all
#' authentication information is retrieved from it.
#' @details If the node specified in the \code{'node'} parameter is a DataONE v2 node
#' or higher, then an authentication token is checked if one exists. If it is readable
#' and not expired, then information for the token is returned. If a valid token does
#' not exist, then the X.509 certificate is checked, if it exists. If it is valid
#' then information is returned for the certificate.
#' @rdname evaluateAuth
#' @param .Object an Authentication Object.
#' @param node A D1Node object.
#' @param ... additional parameters
#' @return A hash containing authentication information.
setGeneric("evaluateAuth", function(.Object, ...) { 
    standardGeneric("evaluateAuth")
})

#' @rdname evaluateAuth
setMethod("evaluateAuth", signature("AuthenticationManager"), function(.Object, node) {
    authInfo <- new("hash")
    # First check if an authentication token is available. 
    # Authentication tokens were implemented in DatONE v2, so if this node is not 
    # v2 or greater, don't send the authToken but look for a x509 certificate instead.
    # This will probably only be useful for development testing, where a single user may
    # be making requests to both v1 and v2 nodes.
    if(.Object@obscured) {
        #warning("DataONE authentication is currenly disabled. See \"?restoreAuth\" to enable it again.")
        authInfo$authMethod <- as.character(NA)
        authInfo$token <- as.character(NA)
        authInfo$cert <- as.character(NA)
        authInfo$node <- node@identifier
        authInfo$valid <- FALSE
        authInfo$expires <- as.character(NA)
        authInfo$expired <- TRUE
        authInfo$subject <- 'public'
        return(authInfo)
    }
    # If DataONE API v2 (tokens are supported), then check if an auth token is available and not expired
    if (node@APIversion >= "v2") {
        # Check if the node we are authentication against is in the DataONE test environment. If yes,
        # then check the option name for a test environment token. Otherwise look for a production
        # token.
        authToken <- getToken(.Object, node)
        # auth token will be null if not set with options(), i.e. getOptions will return NULL; 
        # NA might be set by user
        if(!is.null(authToken) && !is.na(authToken)) {
            tokenName <- attr(authToken, "name")
            tokenInfo <- getTokenDetails(tokenName)
            subject <- tokenInfo[1, 'subject']
            expires <- tokenInfo[1, 'end']
            expired <- tokenInfo[1, 'expired']
            # If the auth token is not valid (e.g. expired) then check for a valid certificate.
            if(subject != 'public' && !expired) {
                authInfo$authMethod <- "token"
                authInfo$token <- authToken
                authInfo$cert <- as.character(NA)
                authInfo$node <- node@identifier
                authInfo$valid <- TRUE
                authInfo$expires <- expires
                authInfo$expired <- expired
                authInfo$subject <- subject
                return(authInfo)
            } #else {
            #message("Checking X.509 certificate...")
            #}
        }
    }
    
    # The authentication token is undefined or invalid, so check if a valid certificate is 
    # available. 
    certInfo <- getCertInfo(.Object)
    subject <- certInfo[1,'subject']
    expires <- certInfo[1,'end']
    expired <- certInfo[1,'expired']
    cert <- certInfo[1,'file']
    if(is.na(cert)) {
        authInfo$authMethod <- as.character(NA)
        authInfo$token <- as.character(NA)
        authInfo$cert <- as.character(NA)
        authInfo$node <- node@identifier
        authInfo$valid <- FALSE
        authInfo$expires <- as.character(NA)
        authInfo$expired <- TRUE
        authInfo$subject <- 'public'
    } else {
        authInfo$authMethod <- "cert"
        authInfo$token <- as.character(NA)
        authInfo$cert <- cert
        authInfo$node <- node@identifier
        if(!expired) {
            authInfo$valid <- TRUE
        } else {
            authInfo$valid <- FALSE
        }
        authInfo$expires <- expires
        authInfo$expired <- expired
        authInfo$subject <- subject
    }
    return(authInfo)
})

#' Get authentication token information
#' @description The DataONE authentication token is read, if it has been
#' set, and the information it contains is returned as a data.frame.
#' @rdname getTokenInfo
#' @param .Object an Authentication Object.
#' @return A data.frame containing information about the authentication token.
#' @export
setGeneric("getTokenInfo", function(.Object) { 
    standardGeneric("getTokenInfo")
})

#' @rdname getTokenInfo
#' @export
setMethod("getTokenInfo", signature("AuthenticationManager"), function(.Object) {
    tdf <- getTokenDetails("dataone_token")
    tdftest <- getTokenDetails("dataone_test_token")
    if(nrow(tdf) == 0 && nrow(tdftest) == 0) {
        message("No tokens are currently defined.")
        # Return one of the empty data.frames, just in case the user is inspecting the values.
        invisible(rbind(tdf))
    } else {
        return(rbind(tdf, tdftest))
    }
})

getTokenDetails <- function(tokenName) {
    authToken <- getOption(tokenName)
    # auth token will be null if not set with options(), i.e. getOptions will return NULL; 
    # NA might be set by user
    if(!is.null(authToken) && !is.na(authToken)) {
        tokenInfo <- parseAuthToken(authToken)
        # issuedAt is UTC
        issuedAt <- tokenInfo[['issuedAt']]
        # Remove the ISO 8601 'T' from the time, R doesn't handle it well.
        issuedAt <- gsub("T", " ", issuedAt)
        startDT <- as.POSIXct(issuedAt, "UTC")
        startSeconds <- as.integer(startDT)
        ttl <- tokenInfo[['ttl']]
        # ttl is 'time to live' in seconds. Calculate expiration date
        # by adding this to 'issuedAt'
        expiresSeconds <- startSeconds + ttl
        # Seconds since the start of the epoch according to R = 1970-01-01
        expiresDT <- as.POSIXct(expiresSeconds, origin="1970-01-01", tz="UTC")
        subject <- tokenInfo[['sub']]
        # Check if the authToken is expired
        if(as.POSIXct(Sys.time(), "UTC") > expiresDT) {
            #message("Your authentication token is expired or invalid. Please login to search.dataone.org and generate a new token.")
            expired <- TRUE
        } else {
            expired <- FALSE
        }
        tdf <- data.frame(name=tokenName, subject=subject, end=expiresDT, expired=expired, stringsAsFactors=FALSE)
    } else {
        #subject <- 'public'
        #expiresDT <- as.POSIXct("1970-01-01 01:01:01", tz="UTC")
        #expired <- TRUE
        #tdf <- data.frame(name=tokenName, subject=subject, end=expiresDT, expired=expired, stringsAsFactors=FALSE)
        tdf <- data.frame(name=character(), subject=character(), end=character(), expired=logical(), stringsAsFactors=FALSE)
    }
    return(tdf)
}

#' Get X.509 Certificate information
#' @description The DataONE X.509 certificate is read, if it is present and the
#' information contained in the certificate is returned as a data.frame.
#' @rdname getCertInfo
#' @param .Object an Authentication Object.
#' @return A data.frame containing information about the X.509 certificate.
#' @export
setGeneric("getCertInfo", function(.Object) { 
    standardGeneric("getCertInfo")
})

#' @rdname getCertInfo
#' @export
setMethod("getCertInfo", signature("AuthenticationManager"), function(.Object) {
    if (requireNamespace("openssl", quietly = TRUE)) {
        # Suppress "deprecated" warning
        suppressWarnings(cm <- CertificateManager())
        suppressWarnings(cert <- getCertLocation(cm))
        # Does cert exist and is it readable?
        if (!is.null(cert) && (file.access(c(cert),4) == 0)) {
            #if(suppressWarnings(isCertExpired(cm))) {
            #  message("Your login certificate is expired. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'")
            #}
            suppressWarnings(expires <- getCertExpires(cm))
            suppressWarnings(expired <- isCertExpired(cm))
            suppressWarnings(subject <- showClientSubject(cm))
        } else {
            # message("Your login certificate is missing or unreadable. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'")
            cert <- as.character(NA)
            expires <- as.POSIXct("1970-01-01 01:01:01", "UTC")
            expired <- TRUE
            subject <- 'public'
        }
    } else {
        # openssl is not available, we are unable to read the Certificate, so print a warning and fall back to unauthenticated
        message("openssl is not installed. You must install the openssl package to enable authenticated operations such as reading private data or uploading data to DataONE repositories.")
        cert <- as.character(NA)
        expires <- as.POSIXct("1970-01-01 01:01:01", "UTC")
        expired <- TRUE
        subject <- 'public'
    }
    tdf <- data.frame(subject=subject, end=expires, expired=expired, file=cert, stringsAsFactors=FALSE)
    return(tdf)
})

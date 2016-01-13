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

#' AuthenticationManager provides mechanisms to validate DataONE authentication,
#' when either a DataONE authentication token or X.509 Certificate is used.
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
#' DataONE version 2.0.0 provides an addition authentication mechanism known as
#' authentication tokens. For information about tokens and instructions for generating
#' a token for use with the DataONE R client, view the package overview by
#' entering the command: \code{'vignette("dataone-overview")'}. DataONE authentication
#' tokens can be obtained by signing in to your DataONE account at https://search.dataone.org.
#' 
#' CILogon recognizes many identity providers, including many universities as well as
#' Google, so most times users new to DataONE can get certificates using one
#' of their existing accounts. For more information about the CILogon service, see 
#' \url{https://cilogon.org/?skin=DataONE} .
#' @slot authInfo value of type \code{"hash"} containing authentication information.
#' @rdname AuthenticationManager-class
#' @aliases AuthenticationManager-class
#' @section Methods:
#' \itemize{
#'  \item{\code{\link{AuthenticationManager}}}{: Create an AuthenticationManager object.}
#'  \item{\code{\link{isAuthValid}}}{: Verify authentication for a member node.}
#'  \item{\code{\link{getAuthToken}}}{: Get the value of the DataONE Authentication Token, if one exists.}
#'  \item{\code{\link{getCert}}}{: Get the DataONE X.509 Certificate location.}
#'  \item{\code{\link{getAuthMethod}}}{: Get the current valid authentication mechanism.}
#'  \item{\code{\link{getAuthSubject}}}{: Get the authentication subject.}
#'  \item{\code{\link{getAuthExpires}}}{: Get the expiration date of the current authentication method.}
#'  \item{\code{\link{isAuthExpired}}}{: CHeck if the currently valid authentication method has reached the expiratin time.}
#'  \item{\code{\link{obscureAuth}}}{: Temporarity disable DataONE authentication.}
#'  \item{\code{\link{restoreAuth}}}{: Restore authentication (after being disabled with \code{obscureAuth}).}
#'  \item{\code{\link{showAuth}}}{: Display all authentication information.}
#' }
#' @seealso \code{\link{dataone}}{ package description.}
#' @import hash
#' @import base64enc
#' @importFrom jsonlite fromJSON
#' @include D1Node.R
#' @export
setClass("AuthenticationManager", slots = c(
    authInfo="hash"
    )
)

#' Create an AuthenticationManager object
#' @description Construct an instance of AuthenticationManager to provide mechanisms to load, verify, and 
#' display DataONE authenticatin information.  
#' @param ... (not yet used)
#' @return the AuthenticationManager object
#' @export
setGeneric("AuthenticationManager", function(...) {
    standardGeneric("AuthenticationManager")
})

#' @rdname AuthenticationManager
setMethod("AuthenticationManager", signature=character(), function() {
    result <- new("AuthenticationManager")
    result@authInfo <- new("hash")
    result@authInfo$authMethod <- as.character(NA)
    result@authInfo$token <- as.character(NA)
    result@authInfo$cert <- as.character(NA)
    # The node that authentication was checked for. 
    result@authInfo$node <- as.character(NA)
    # Did we perform the check?
    result@authInfo$init <- FALSE
    options(D1AuthObscured = FALSE)
    return(result)
})

#' Verify authentication for a member node.
#' @rdname isAuthValid
#' @aliases isAuthValid
#' @description The currently used DataONE client authentication method (either tokens or X.509 certificates)
#' is checked and verified for the specified node (either CN or MN). If an authentication token is availalbe
#' via the R options facility, it will be used i.e. available via getOption("authentication_token").  However, 
#' authentication tokens can only be used for DataONE v2 or higher nodes. X.509 certificates can be used 
#' with DataONE v1 or higher nodes. 
#' See the \emph{"dataone"} vignette \emph{"dataone-overview"} for more information on authentication.
#' @param x An AuthenticationManager instance
#' @param node The node object (MNode or CNode) that authentication is being checked for.
#' @param ... (Not yet used)
#' @return A logical value: TRUE if authentication is valid, false if not.
#' @export
setGeneric("isAuthValid", function(x, node, ...) { 
    standardGeneric("isAuthValid")
})

#' @rdname isAuthValid
#' @export
setMethod("isAuthValid", signature("AuthenticationManager", "D1Node"), function(x, node) {
  # First check if an authentication token is available. 
  # Authentication tokens were implemented in DatONE v2, so if this node is not
  # v2 or greater, don't send the authToken but look for a x509 certificate instead.
  # This will probably only be useful for development testing, where a single user may
  # be making requests to both v1 and v2 nodes.
  # Currently, if a token is found it is considered to be valid. When an R package is
  # available to decode JSON Web Tokens, then we will be able to check the expiration
  # date of the token.
  if(getOption("D1AuthObscured")) {
    #warning("DataONE authentication is currenly disabled. See \"?restoreAuth\" to enable it again.")
    return(FALSE)
  }
  if (!is.null(node) && node@APIversion >= "v2") {
    authToken <- getOption("authentication_token")
    # auth token will be null if not set with options(); NA might be set by user
    # TODO: verify token when JWT available.
    if(!is.null(authToken) && !is.na(authToken)) {
      x@authInfo$authMethod <- "token"
      x@authInfo$token <- authToken
      x@authInfo$cert <- as.character(NA)
      x@authInfo$node <- node@identifier
      tokenInfo <- parseAuthToken(authToken)
      # issuedAt is GMT
      issuedAt <- tokenInfo[['issuedAt']]
      # Remove the ISO 8601 'T' from the time, R doesn't handle it well.
      issuedAt <- gsub("T", " ", issuedAt)
      startDT <- as.POSIXct(issuedAt, "GMT")
      startSeconds <- as.integer(startDT)
      ttl <- tokenInfo[['ttl']]
      # ttl is 'time to live' in seconds. Calculate expiration date
      # by adding this to 'issuedAt'
      expiresSeconds <- startSeconds + ttl
      # Seconds since the start of the epoch according to R = 1970-01-01
      expiresDT <- as.POSIXct(expiresSeconds, origin="1970-01-01", tz="GMT")
      x@authInfo$expires <- expiresDT
      x@authInfo$subject <- tokenInfo[['sub']]
      # Check if the authToken is expired
      if(as.POSIXct(Sys.time(), "GMT") > x@authInfo$expires) {
        x@authInfo$init <- FALSE
        warning("Your authentication token is expired or invalid. Please login to search.dataone.org and generate a new token.")
        return(FALSE)
      } else {
        x@authInfo$init <- TRUE
        return(TRUE)
      }
    }
  }
  
  # The authentication token is undefined or invalid, so check if a valid certificate is 
  # available.
  if (check4PKI()) {
    # Suppress "deprecated" warning
    cm <- suppressWarnings(CertificateManager())
    cert <- getCertLocation(cm)
    if (!is.null(cert) && (file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
      x@authInfo$authMethod <- "cert"
      x@authInfo$token <- as.character(NA)
      x@authInfo$cert <- cert
      x@authInfo$node <- node@identifier
      x@authInfo$init <- TRUE
      x@authInfo$expires <- getCertExpires(cm)
      x@authInfo$subject <- showClientSubject(cm)
      return(TRUE)
    } else {
      # The certificate is invalid or unreadable, so fall back to unauthenticated?
      authWarn <- getOption("auth_warn")
      if(!is.null(authWarn) && !is.na(authWarn)) {
        if(authWarn) {
          warning("Your login certificate is expired or invalid. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'")
        }
      } else {
        warning("Your login certificate is expired or invalid. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'")
      }
      x@authInfo$authMethod <- as.character(NA)
      x@authInfo$token <- as.character(NA)
      x@authInfo$cert <- as.character(NA)
      x@authInfo$node <- node@identifier
      x@authInfo$init <- TRUE
      return(FALSE)
    }
  } else {
    # PKIplus is not available, so print a warning and fall back to unauthenticated
    authWarn <- getOption("auth_warn")
    if(!is.null(authWarn) && !is.na(authWarn)) {
      if(authWarn) {
        warning("PKIplus not installed. You must install the PKIplus package to enable authenticated operations such as reading private data or uploading data to DataONE repositories.")
        warning("To install PKIplus, try 'drat::addRepo(\"NCEAS\"); install.packages(\"PKIplus\")`")
      } else {
        warning("PKIplus not installed. You must install the PKIplus package to enable authenticated operations such as reading private data or uploading data to DataONE repositories.")
        warning("To install PKIplus, try 'drat::addRepo(\"NCEAS\"); install.packages(\"PKIplus\")`")
      }
    }
    x@authInfo$authMethod <- as.character(NA)
    x@authInfo$token <- as.character(NA)
    x@authInfo$cert <- as.character(NA)
    x@authInfo$node <- node@identifier
    x@authInfo$init <- TRUE
    return(FALSE)
  }
})

#' Get the value of the DataONE Authentication Token, if one exists.
#' @rdname getAuthToken
#' @aliases getAuthToken
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used)
#' @return The current authentication token.
#' @export
setGeneric("getAuthToken", function(x, ...) { 
    standardGeneric("getAuthToken")
})

#' @rdname getAuthToken
setMethod("getAuthToken", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  }
  if(is.na(x@authInfo[["authMethod"]])) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  } 
  return(x@authInfo[["token"]])
})
    
#' Get the DataONE X.509 Certificate location.
#' @rdname getCert
#' @aliases getCert
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used)
#' @return The filename of the current DataONE X.509 Certificate if it is available.
#' @export
setGeneric("getCert", function(x, ...) { 
  standardGeneric("getCert")
})

#' @rdname getCert
setMethod("getCert", signature("AuthenticationManager"), function(x) { 
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  }
  if(is.na(x@authInfo$authMethod)) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  return(x@authInfo[["cert"]])
})

#' Get the current valid authentication mechanism.
#' @rdname getAuthMethod
#' @aliases getAuthMethod
#' @param x A AuthenticationManager instance
#' @param ... (Not yet used)
#' @return The current authentication mechanism as a character string, either "token" or "cert".
#' @export
setGeneric("getAuthMethod", function(x, ...) { 
  standardGeneric("getAuthMethod")
})

#' @rdname getAuthMethod
setMethod("getAuthMethod", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  }
  if(!x@authInfo$init) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  
  return(x@authInfo[["authMethod"]])
})

#' Get the authentication subject.
#' @rdname getAuthSubject
#' @aliases getAuthSubject
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used)
#' @return the DataONE Subject that is your client's identity
#' @export
setGeneric("getAuthSubject", function(x, ...) { 
  standardGeneric("getAuthSubject")
})

#' @rdname getAuthSubject
#' @export
setMethod("getAuthSubject", signature("AuthenticationManager"), function(x) {
  PUBLIC <- "public"
  if(getOption("D1AuthObscured")) {
    return(as.character(PUBLIC))
  }
  
  if(!x@authInfo$init) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  
  # Authentication is either uninitialized or no method present (no cert or token available)
  if(is.na(x@authInfo[["authMethod"]])) {
    return(as.character(PUBLIC))
  }
  # TODO: set client subject for a token when JWT package is available.
  if(x@authInfo$authMethod == "token") {
    return(x@authInfo$subject)
  } else {
    cm <- suppressWarnings(CertificateManager())
    return(showClientSubject(cm))
  }
})

#' Get the expiration date of the current authentication method.
#' @description The expiration date of the current authentication method, either
#' authentication token or X.509 certificate, is returned as a Greenich Mean Time (GMT) value.
#' @rdname getAuthExpires
#' @aliases getAuthExpires
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used.)
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("getAuthExpires", function(x, ...) { 
  standardGeneric("getAuthExpires")
})

#' @rdname getAuthExpires
#' @export
setMethod("getAuthExpires", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  } 
  if(!x@authInfo$init) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  # TODO: set client subject for a token when JWT package is available.
  if(x@authInfo$authMethod == "token") {
    return(x@authInfo$expires)
  } else {
    # Turn off warnings the Deprecated msg doesn't get printed
    cm <- suppressWarnings(CertificateManager())
    return(getCertExpires(cm))
  }
})
#' CHeck if the currently valid authentication method has reached the expiratin time.
#' @rdname isAuthExpired
#' @aliases isAuthExpired
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used.)
#' @return A logical value: TRUE if authenentication has expired, FALSE if not.
#' @export
setGeneric("isAuthExpired", function(x, ...) { 
  standardGeneric("isAuthExpired")
})

#' @rdname isAuthExpired
#' @export
setMethod("isAuthExpired", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(TRUE)
  } 
  if(!x@authInfo$init) {
    warning("Please call isAuthValid() before calling this method.")
    return(TRUE)
  }
  # TODO: set client subject for a token when JWT package is available.
  # Until JWT is available, we have to assume that a token has not expired. 
  if(x@authInfo$authMethod == "token") {
    if(as.POSIXct(Sys.time(), "GMT") > x@authInfo$expires) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    # Turn off warnings so that the Deprecated msg doesn't get printed
    cm <- suppressWarnings(CertificateManager())
    return(isCertExpired(cm))
  }
})

#' Temporarity disable DataONE authentication.
#' @description Calling \code{obscureAuth} temporarily disables authentication so that
#' @details This method is intended to be used for authentication testing.
#' \code{isAuthValid} will return FALSE. Authentication can be re-enabled by calling
#' \code{restoreAuth}.
#' @rdname obscureAuth
#' @aliases obscureAuth
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used.)
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("obscureAuth", function(x, ...) { 
  standardGeneric("obscureAuth")
})

#' @rdname obscureAuth
#' @export
setMethod("obscureAuth", signature("AuthenticationManager"), function(x) {
  options(D1AuthObscured = TRUE)
})
#' Restore authentication (after being disabled with \code{obscureAuth}).
#' @rdname restoreAuth
#' @aliases restoreAuth
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used.)
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("restoreAuth", function(x, ...) { 
  standardGeneric("restoreAuth")
})

#' @rdname restoreAuth
#' @export
setMethod("restoreAuth", signature("AuthenticationManager"), function(x) {
  options(D1AuthObscured = FALSE)
})

#' Display all authentication information
#' @rdname showAuth
#' @aliases showAuth
#' @param x an AuthenticationManager instance
#' @param ... (Not yet used.)
#' @export
setGeneric("showAuth", function(x, ...) { 
  standardGeneric("showAuth")
})

#' @rdname showAuth
#' @export
setMethod("showAuth", signature("AuthenticationManager"), function(x) {
  obscured <- getOption("D1AuthObscured")
  if(obscured) {
    message("DataONE authentication is currently disabled. See \"?restoreAuth\" to re-enable authentication")
  }
  if(!x@authInfo$init) {
    message("DataONE authentication has not been initialized. See \"?isAuthValid\"")
  } else {
    message(sprintf("Authentication method: %s", x@authInfo$authMethod))
    if(!is.na(x@authInfo$authMethod) && x@authInfo$authMethod == "cert") {
      message(sprintf("Certification location: %s", x@authInfo$cert))
    }
    message(sprintf("Authenticated node: %s", x@authInfo$node))
  }
})

parseAuthToken <- function(authToken) {
  # TODO: put in checks for a valid token format
  payload <- unlist(strsplit(authToken, "\\."))[[2]]
  payloadRaw <- base64decode(payload)
  payloadChar <- rawToChar(payloadRaw)
  tokenInfo <- fromJSON(payloadChar)
  return(tokenInfo)
}

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

#' AuthenticationManager provides mechanisms to 
#' @description AuthenticationManager
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
#' entering the command: \code{'vignette("dataone-overview")'}
#' 
#' CILogon recognizes many identity providers, including many universities as well as
#' Google, so most times users new to DataONE can get certificates using one
#' of their existing accounts. For more information about the CILogon service, see 
#' \url{https://cilogon.org/?skin=DataONE} .
#' 
#' @slot authInfo value of type \code{"hash"} containing authentication information.
#' @rdname AuthenticationManager-class
#' @import hash
#' @examples
#' am <- AuthenticationManager()
setClass("AuthenticationManager", slots = c(
    authInfo="hash"
    )
)

#' Create an AuthenticationManager object
#' @description Construct an instance of AuthenticationManager to provide mechanisms to load, verify, and 
#' display DataONE authenticatin information.  
#' @return the AuthenticationManager object
#' @export
setGeneric("AuthenticationManager", function(...) {
    standardGeneric("AuthenticationManager")
})

#' @describeIn AuthenticationManager
setMethod("AuthenticationManager", signature=character(), function() {
    result <- new("AuthenticationManager")
    result@authInfo <- new("hash")
    result@authInfo$authMethod <- as.character(NA)
    result@authInfo$token <- as.character(NA)
    result@authInfo$cert <- as.character(NA)
    options(D1AuthObscured = FALSE)
    return(result)
})

#' Verify authentication for a member node.
#' @rdname isAuthValid
#' @aliases isAuthValid
#' @description The currently used authentication methonism (either tokens or X.509 certificates)
#' is checked and verified for the specified member node. If an authentication token is availalbe
#' via the dataone package Session Configuration, it will be used.  However, authentication tokens can only be used for
#' DataONE v2.0 or higher nodes. X.509 certificates can be used with DataONE v1.0.0 or higher member nodes.
#' See the \emph{"dataone"} vignette \emph{"dataone-overview"} for more information on authentication.
#' @param x a AuthenticationManager instance
#' @return A logical value: TRUE if authentication is valid, false if not.
#' @export
setGeneric("isAuthValid", function(x, node, ...) { 
    standardGeneric("isAuthValid")
})

#' @describeIn isAuthValid
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
      return(TRUE)
    }
  }
  
  # The authentication token is undefined or invalid, so check if a valid certificate is 
  # available.
  if (check4PKI()) {
    # Check if a valid certificate is available
    oldWarnLevel <- getOption("warn")
    # Turn off warnings so that the Deprecated msg doesn't get printed
    options(warn = -1)
    cm <- CertificateManager()
    # Restore warnings to previous level
    options(warn = oldWarnLevel)
    cert <- getCertLocation(cm)
    if (!is.null(cert) && (file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
      x@authInfo$authMethod <- "cert"
      x@authInfo$token <- as.character(NA)
      x@authInfo$cert <- cert
      return(TRUE)
    } else {
      # The certificate is invalid or unreadable, so fall back to unauthenticated?
      warning("Your login certificate is expired or invalid. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'")
      x@authInfo$authMethod <- as.character(NA)
      x@authInfo$token <- as.character(NA)
      x@authInfo$cert <- as.character(NA)
      return(FALSE)
    }
  } else {
    # PKIplus is not available, so print a warning and fall back to unauthenticated
    warning("PKIplus not installed. You must install the PKIplus package to enable authenticated operations such as reading private data or uploading data to DataONE repositories.")
    warning("To install PKIplus, try 'drat::addRepo(\"NCEAS\"); install.packages(\"PKIplus\")`")
    x@authInfo$authMethod <- as.character(NA)
    x@authInfo$token <- as.character(NA)
    x@authInfo$cert <- as.character(NA)
    return(FALSE)
  }
})

#' Get the value of the DataONE Authentication Token, if one exists.
#' @rdname getAuthToken
#' @aliases getAuthToken
#' @param x an AuthenticationManager instance
#' @return The current authentication token.
#' @export
setGeneric("getAuthToken", function(x, ...) { 
    standardGeneric("getAuthToken")
})

#' @describeIn getAuthToken
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
#' @return The filename of the current DataONE X.509 Certificate if it is available.
#' @export
setGeneric("getCert", function(x, ...) { 
  standardGeneric("getCert")
})

#' @describeIn getCert
setMethod("getCert", signature("AuthenticationManager"), function(x) { 
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  }
  if(is.na(x@authInfo[["authMethod"]])) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  return(x@authInfo[["cert"]])
})

#' Get the current valid authentication mechanism.
#' @rdname getAuthMethod
#' @aliases getAuthMethod
#' @param x a getAuthMethod instance
#' @return The current authentication mechanism as a character string, either "token" or "cert".
#' @export
setGeneric("getAuthMethod", function(x, ...) { 
  standardGeneric("getAuthMethod")
})

#' @describeIn getAuthMethod
setMethod("getAuthMethod", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  }
  if(is.na(x@authInfo[["authMethod"]])) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  return(x@authInfo[["authMethod"]])
})

#' Get the authentication subject.
#' @rdname getAuthSubject
#' @aliases getAuthSubject
#' @param x an AuthenticationManager instance
#' @return the DataONE Subject that is your client's identity
#' @export
setGeneric("getAuthSubject", function(x, ...) { 
  standardGeneric("getAuthSubject")
})

#' @describeIn getAuthSubject
setMethod("getAuthSubject", signature("AuthenticationManager"), function(x) {
  PUBLIC <- "public"
  if(getOption("D1AuthObscured")) {
    return(as.character(PUBLIC))
  }
  authMethod <- getAuthMethod(x)
  if(is.na(authMethod)) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(PUBLIC))
  }
  # TODO: set client subject for a token when JWT package is available.
  if(authMethod == "token") {
    return(as.character(NA))
  } else {
    oldWarnLevel <- getOption("warn")
    # Turn off warnings so that the Deprecated msg doesn't get printed
    options(warn = -1)
    cm <- CertificateManager()
    options(warn = oldWarnLevel)
    return(showClientSubject(cm))
  }
})

#' Get DataONE Identity as Stored in the CILogon Certificate.
#' @rdname getAuthExpires
#' @aliases getAuthExpires
#' @param x an Authentication instance
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("getAuthExpires", function(x, ...) { 
  standardGeneric("getAuthExpires")
})

#' @describeIn AuthenticationManager
setMethod("getAuthExpires", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(as.character(NA))
  } 
  authMethod <- getAuthMethod(x)
  if(is.na(authMethod)) {
    warning("Please call isAuthValid() before calling this method.")
    return(as.character(NA))
  }
  # TODO: set client subject for a token when JWT package is available.
  if(authMethod == "token") {
    return(as.character(NA))
  } else {
    # Turn off warnings the Deprecated msg doesn't get printed
    oldWarnLevel <- getOption("warn")
    options(warn = -1)
    cm <- CertificateManager()
    options(warn = oldWarnLevel)
    return(getCertExpires(cm))
  }
})
#' CHeck if the currently valid authentication method has reached the expiratin time.
#' @rdname isAuthExpired
#' @aliases isAuthExpired
#' @param x an Authentication instance
#' @return A logical value: TRUE if authenentication has expired, FALSE if not.
#' @export
setGeneric("isAuthExpired", function(x, ...) { 
  standardGeneric("isAuthExpired")
})

#' @describeIn AuthenticationManager
setMethod("isAuthExpired", signature("AuthenticationManager"), function(x) {
  if(getOption("D1AuthObscured")) {
    return(TRUE)
  } 
  authMethod <- getAuthMethod(x)
  if(is.na(authMethod)) {
    warning("Please call isAuthValid() before calling this method.")
    return(TRUE)
  }
  # TODO: set client subject for a token when JWT package is available.
  # Until JWT is available, we have to assume that a token has not expired. 
  if(authMethod == "token") {
    return(TRUE)
  } else {
    # Turn off warnings so that the Deprecated msg doesn't get printed
    oldWarnLevel <- getOption("warn")
    options(warn = -1)
    cm <- CertificateManager()
    options(warn = oldWarnLevel)
    return(isCertExpired(cm))
  }
})

#' Get DataONE Identity as Stored in the CILogon Certificate.
#' @rdname getAuthExpires
#' @aliases getAuthExpires
#' @param x an Authentication instance
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("obscureAuth", function(x, ...) { 
  standardGeneric("obscureAuth")
})

#' @describeIn AuthenticationManager
setMethod("obscureAuth", signature("AuthenticationManager"), function(x) {
  options(D1AuthObscured = TRUE)
})
#' Get DataONE Identity as Stored in the CILogon Certificate.
#' @rdname getAuthExpires
#' @aliases getAuthExpires
#' @param x an Authentication instance
#' @return The expiration date for the current authentication mechanism being used.
#' @export
setGeneric("restoreAuth", function(x, ...) { 
  standardGeneric("restoreAuth")
})

#' @describeIn AuthenticationManager
setMethod("restoreAuth", signature("AuthenticationManager"), function(x) {
  options(D1AuthObscured = FALSE)
})

#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2015
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

#' GET a resource with authenticated credentials if available.
#' @description Retrieve the data at a URL using an HTTP GET request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' openssl package. If the openssl package is not installed, then the request falls back
#' to an unauthenticated request, which may fail due to insufficient permissions.
#' Configuration options for httr/RCurl can be passed using the normal config()
#' mechanisms to generate a config option. Use httr_options() to see a complete list
#' of available options.
#' @param url The URL to be accessed via authenticated GET.
#' @param nconfig HTTP configuration options as used by curl, defaults to empty list
#' @param node The D1Node object that the request will be made to.
#' @param path Path to a file to write object to
#' @return the response object from the method
#' @import httr
auth_get <- function(url, nconfig=config(), node, path = NULL) {
  response <- NULL
  tryCatch({
    if (is.null(path)) {
      write_path <- NULL
    } else {
      write_path <- httr::write_disk(path, overwrite = FALSE)
    }
    if (missing(url) || missing(node)) {
      stop("Error: url or node is missing. Please report this error.")
    }
    am <- AuthenticationManager()
    if(isAuthValid(am, node)) {
      if(getAuthMethod(am, node) == "token") {
        # Authentication will use an authentication token.
        authToken <- getToken(am, node)
        response <- GET(url, config = nconfig, user_agent(get_user_agent()), add_headers(Authorization = sprintf("Bearer %s", authToken)), write_path)
      } else {
        # Authentication will use a certificate.
        cert <- getCert(am)
        new_config <- c(nconfig, config(sslcert = cert))
        response <- GET(url, config = new_config, user_agent(get_user_agent()), write_path)
      }
    } else {
      # Send request as the public user
      # Warn the user if their auth token or certificate has expired. The regular auth checks
      # are designed to check for and return whatever valid auth mechanism is used, and are not
      # designed to find an invalid, i.e. expired one, so we have to perform these checks manually.
      # First check if a token is present, for the appropriate D1 environment, i.e. v1 vs v2, production
      # vs development.
      authToken <- getToken(am, node)
      if(!is.null(authToken)) {
        tokenInfo <- getTokenDetails(attr(authToken, "name"))
        if(tokenInfo$expired) {
          msg <- "You attempted this operation with an expired token, so you were not authenticated."
          msg <- paste0(msg, "\nYou may wish to try again with a valid token.")
          msg <- paste0(msg, "\nAttempting to perform this operation as the public user without being authenticated.")
          message(msg)
        }
      } else {
        # If no token, then check for a certificate
        certInfo <- getCertInfo(am)
        # A certificate exists
        if(!is.na(certInfo$file)) {
          if(certInfo$expired) {
            msg <- sprintf("You attempted this operation with an expired certificate located at %s", certInfo$file)
            msg <- paste0(msg, sprintf("\nso you were not authenticated. You may wish to try again with a valid certificate."))
            msg <- paste0(msg, sprintf("\nAttempting to perform this operation as the public user without being authenticated."))
            message(msg)
          }
        }
      }
      
      response <- GET(url, config=nconfig, user_agent(get_user_agent()), write_path)   # the anonymous access case
    }
    rm(am)
    return(response)
  },
  error = function(cond){
    node <- newXMLNode("error")
    newXMLNode("description", parent = node, text = as.character(cond))
    xmlAttrs(node)["name"] <- "InternalServerError"
    xmlAttrs(node)["errorCode"] <- "500"
    content <- saveXML(node)
    response <- structure(list(url = url,
                               status_code = "500",
                               headers = list("content-type" = "text/xml"),
                               content = charToRaw(content)),
                          class = "response") 
    return(response)
  })
}

#' Send a http HEAD request for a resource with authenticated credentials if available.
#' @description Retrieve http header information for a URL using an HTTP HEAD request 
#' using authentication credentials provided in a client certificate or token.  
#' Authenticated access depends on the suggested openssl package. If the openssl 
#' package is not installed, then the request falls back to an unauthenticated request, 
#' which may fail due to insufficient permissions.
#' Configuration options for httr/RCurl can be passed using the normal config()
#' mechanisms to generate a config option. Use httr_options() to see a complete list
#' of available options. Note: The HEAD method is identical to GET except that the server MUST 
#' NOT return a message-body in the response.
#' @param url The URL to be accessed via authenticated HEAD.
#' @param nconfig HTTP configuration options as used by curl, defaults to empty list
#' @param node The D1Node object that the request will be made to.
#' @return the response object from the method
#' @import httr
auth_head <- function(url, nconfig=config(), node) {
  response <- NULL
  if (missing(url) || missing(node)) {
      stop("Error: url or node is missing. Please report this error.")
  }
  am <- AuthenticationManager()
  if(isAuthValid(am, node)) {
    if(getAuthMethod(am, node) == "token") {
      # Authentication will use an authentication token.
      authToken <- getToken(am, node)
      response <- HEAD(url, config = nconfig, user_agent(get_user_agent()), add_headers(Authorization = sprintf("Bearer %s", authToken)))
    } else {
      # Authenticatin will use a certificate.
      cert <- getCert(am)
      new_config <- c(nconfig, config(sslcert = cert))
      response <- HEAD(url, config = new_config, user_agent(get_user_agent()))
    }
  } else {
    # Send request as the public user
    message("Attempting to call as public user without being authenticated.")
    response <- HEAD(url, config=nconfig, user_agent(get_user_agent()))   # the anonymous access case
  }
  rm(am)
  
  return(response)
}
  
#' POST, PUT, or DELETE a resource with authenticated credentials.
#' @description POST, PUT, or DELETE data to a URL using an HTTP request using authentication credentials 
#' provided in a client authentication, either via authentication token or certificate.
#' If the user does not have a valid token or certificate, request fails.
#' @param method a string indicating which HTTP method to use (post, put, or delete)
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @param node The D1Node object that the request will be made to.
#' @return the response object from the method
#' @import httr
auth_put_post_delete <- function(method, url, encode="multipart", body=as.list(NA), node) {
  
  am <- AuthenticationManager()
  if(!missing(node) && isAuthValid(am, node)) {
    if (is.na(body[1])) {
      body = FALSE
    }
    if(getAuthMethod(am, node) == "token") {
      # Authentication will use an authentication token.
      authToken <- getToken(am, node)
      switch(method,
             post={
               response=POST(url, encode=encode, body=body, config(tcp_keepalive = as.numeric(1)), add_headers(Authorization = sprintf("Bearer %s", authToken)), user_agent(get_user_agent()))
               return(response)
             },
             put={
               response=PUT(url, encode=encode, body=body, add_headers(Authorization = sprintf("Bearer %s", authToken)), user_agent(get_user_agent()))
               return(response)
             },
             delete={
               response=DELETE(url, encode=encode, body=body, add_headers(Authorization = sprintf("Bearer %s", authToken)), user_agent(get_user_agent()))
               return(response)},
             {
               stop('Method not supported.')
             }
      )
    } else {
      # Certificate will be used
      cert <- getCert(am)
      switch(method,
             post={
               response=POST(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent()))
               return(response)
             },
             put={
               response=PUT(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent()))
               return(response)
             },
             delete={
               response=DELETE(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent()))
               return(response)},
             {
               stop('Method not supported.')
             }
      )
    }
  } else {
    stop("Unable to perform the operation as unauthenticated (public) user. See vignette(\"dataone-overview\") for more information")
  }
}

#' POST a resource with authenticated credentials.
#' @description POST data to a URL using an HTTP POST request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' openssl package. If the openssl package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated POST
#' @param encode the type of encoding to use for the POST body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the POST request
#' @param node The D1Node object that the request will be made to.
#' @return the HTTP response from the request
#' @import httr
auth_post <- function(url, encode="multipart", body=as.list(NA), node) {
    response <- auth_put_post_delete("post", url, encode, body, node)
    return(response)
}

#' PUT a resource with authenticated credentials.
#' @description PUT data to a URL using an HTTP PUT request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' openssl package. If the openssl package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @param node The D1Node object that the request will be made to.
#' @return the HTTP response from the request
#' @import httr
auth_put <- function(url, encode="multipart", body=as.list(NA), node) {
    response <- auth_put_post_delete("put", url, encode, body, node)
    return(response)
}

#' DELETE a resource with authenticated credentials.
#' @description DELETE data at a URL using an HTTP DELETE request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' openssl package. If the openssl package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated DELETE
#' @param encode the type of encoding to use for the DELETE body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the DELETE request
#' @param node The D1Node object that the request will be made to.
#' @return the HTTP response from the request
#' @import httr
auth_delete <- function(url, encode="multipart", body=as.list(NA), node) {
    response <- auth_put_post_delete("delete", url, encode, body, node)
    return(response)
}

#' User agent string
#' @description Get a string representation of the user agent to be sent to the server along
#' with other request details.
#' @importFrom utils sessionInfo
#' @importFrom utils installed.packages
#' @importFrom utils packageDescription
get_user_agent <- function() {
    info <- sessionInfo()
    
    # Get the version number of httr
    # First check if the package is installed
    if ("httr" %in% installed.packages()[,'Package']) {
      httrDesc <- packageDescription("httr")
      if(!all(is.na(httrDesc))) {
        httrVersion <- httrDesc$Version
      }
    } else {
      stop("Package httr is not installed and is required by the dataone package.")
    }
    
    dataoneVersion <- packageDescription("dataone")$Version
    local_agent <- sprintf("dataone/%s R/%s httr/%s", 
                             dataoneVersion,
                             paste(info$R.version$major, info$R.version$minor, sep="."),
                             httrVersion)
    return(local_agent)
}
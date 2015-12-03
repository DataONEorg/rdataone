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
#' 
#' Retrieve the data at a URL using an HTTP GET request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKIplus package. If the PKIplus package is not installed, then the request falls back
#' to an unauthenticated request, which may fail due to insufficient permissions.
#' Configuration options for httr/RCurl can be passed using the normal config()
#' mechanisms to generate a config option. Use httr_options() to see a complete list
#' of available options.
#' @param url The URL to be accessed via authenticated GET.
#' @param nconfig HTTP configuration options as used by curl, defaults to empty list
#' @return the response object from the method
#' @import httr
auth_get <- function(url, nconfig=config(), node=NULL) {
  response <- NULL
  am <- AuthenticationManager()
  if(isAuthValid(am, node)) {
    if(getAuthMethod(am) == "token") {
      # Authentication will use an authentication token.
      authToken <- getAuthToken(am)
      response <- GET(url, config = nconfig, user_agent(get_user_agent()), add_headers(Authorization = sprintf("Bearer %s", authToken)))
    } else {
      # Authenticatin will use a certificate.
      cert <- getCert(am)
      new_config <- c(nconfig, config(sslcert = cert))
      response <- GET(url, config = new_config, user_agent(get_user_agent()))
    }
  } else {
    # Send request as the public user
    warning("Attempting to call as public user without being authenticated.")
    response <- GET(url, config=nconfig, user_agent(get_user_agent()))   # the anonymous access case
  }
  rm(am)
  
  return(response)
}

#' POST, PUT, or DELETE a resource with authenticated credentials.
#' 
#' POST, PUT, or DELETE data to a URL using an HTTP request using authentication credentials 
#' provided in a client authentication, either via authentiction token or certificate.
#' If the user does not have a valid token or certificate, request fails.
#' @param method a string indicating which HTTP method to use (post, put, or delete)
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @return the response object from the method
#' @import httr
auth_put_post_delete <- function(method, url, encode="multipart", body=as.list(NA), node=NULL) {
  
  am <- AuthenticationManager()
  if(isAuthValid(am, node)) {
    if(getAuthMethod(am) == "token") {
      # Authentication will use an authentication token.
      authToken <- getAuthToken(am)
      switch(method,
             post={
               response=POST(url, encode=encode, body=body, add_headers(Authorization = sprintf("Bearer %s", authToken)), user_agent(get_user_agent()))
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
    stop("Unable to perform the operation as unauthenticated (public) user.")
  }
}

#' POST a resource with authenticated credentials.
#' 
#' POST data to a URL using an HTTP POST request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKIplus package. If the PKIplus package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated POST
#' @param encode the type of encoding to use for the POST body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the POST request
#' @return the HTTP response from the request
#' @import httr
auth_post <- function(url, encode="multipart", body=as.list(NA), node=NULL) {
    response <- auth_put_post_delete("post", url, encode, body, node)
    return(response)
}

#' PUT a resource with authenticated credentials.
#' 
#' PUT data to a URL using an HTTP PUT request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKIplus package. If the PKIplus package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @return the HTTP response from the request
#' @import httr
auth_put <- function(url, encode="multipart", body=as.list(NA), node=NULL) {
    response <- auth_put_post_delete("put", url, encode, body, node)
    return(response)
}

#' DELETE a resource with authenticated credentials.
#' 
#' DELETE data at a URL using an HTTP DELETE request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKIplus package. If the PKIplus package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated DELETE
#' @param encode the type of encoding to use for the DELETE body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the DELETE request
#' @return the HTTP response from the request
#' @import httr
auth_delete <- function(url, encode="multipart", body=as.list(NA), node=NULL) {
    response <- auth_put_post_delete("delete", url, encode, body, node)
    return(response)
}

#' Determine if the PKI package is installed
check4PKI <- function() {
    if (!requireNamespace("PKIplus", quietly = TRUE)) {
        invisible(FALSE)
    } else {
        invisible(TRUE)
    }
}

#' User agent string
#' 
#' Get a string representation of the user agent to be sent to the server along
#' with other request details.
## @import curl
## @import httr
get_user_agent <- function() {
    info <- sessionInfo()
    local_agent <- sprintf("dataone/%s R/%s", 
                        info$otherPkgs$dataone$Version, 
                        paste(info$R.version$major, info$R.version$minor, sep="."))
    #local_agent <- sprintf("dataone/%s curl/%s httr/%s R/%s", 
    #                    info$otherPkgs$dataone$Version, 
    #                    info$otherPkgs$curl$Version,
    #                    info$otherPkgs$httr$Version,
    #                    paste(info$R.version$major, info$R.version$minor, sep="."))
    return(local_agent)
}
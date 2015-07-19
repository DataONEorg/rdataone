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
#' PKI package. If the PKI package is not installed, then the request falls back
#' to an unauthenticated request, which may fail due to insufficient permissions.
#' Configuration options for httr/RCurl can be passed using the normal config()
#' mechanisms to generate a config option. Use httr_options() to see a complete list
#' of available options.
#' @param url The URL to be accessed via authenticated GET.
#' @param config HTTP configuration options as used by curl, defaults to empty list
#' @return the response object from the method
#' @import httr
auth_get <- function(url, config=config()) {
    cert <- NULL
    auth <- F
    response <- NULL
    if (check4PKI()) {
        # Check if a valid certificate is available
        cm = CertificateManager()
        cert <- getCertLocation(cm)
        if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
            new_config <- c(config, config(sslcert = cert))
            response <- GET(url, config = new_config, user_agent(get_user_agent()))
        } else {
            # The certificate is invalid or unreadable, so fall back to unauthenticated?
            warning("Your login certificate is expired or invalid. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'. Attempting call as public user without being authenticated.")
            response <- GET(url, user_agent(get_user_agent()))   # the anonymous access case
        }
    } else {
        # PKI is not available, so print a warning and fall back to unauthenticated
        warning("PKIext not installed. You must install the PKI package to enable authenticated operations such as reading private data or uploading data to DataONE repositories. Attempting call as public user without being authenticated.")
        response <- GET(url, user_agent(get_user_agent()))   # the anonymous access case
    }
    return(response)
}

#' POST, PUT, or DELETE a resource with authenticated credentials.
#' 
#' POST, PUT, or DELETE data to a URL using an HTTP request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKI package. If the PKI package is not installed, then the request fails.
#' @param method a string indicating which HTTP method to use (post, put, or delete)
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @return the response object from the method
#' @import httr
auth_put_post_delete <- function(method, url, encode="multipart", body=as.list(NA)) {
    cert <- NULL
    auth <- F
    if (check4PKI()) {
        # Check if a valid certificate is available
        cm = CertificateManager()
        cert <- getCertLocation(cm)
        if ((file.access(c(cert),4) == 0) && !isCertExpired(cm)) {
            switch(method,
                post <- POST(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent())),
                put <- PUT(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent())),
                delete <- DELETE(url, encode=encode, body=body, config=config(sslcert = cert), user_agent(get_user_agent()))
            )
        } else {
            # The certificate is invalid or unreadable, so fall back to unauthenticated?
            stop("Your login certificate is expired or invalid. You must login again via CILogon using 'dataone::downloadCert(CertificateManager())'.")
        }
    } else {
        # PKI is not available, so throw an error
        stop("PKIext not installed. You must install the PKI package to enable authenticated operations such as uploading data to DataONE repositories.")
    }
}

#' POST a resource with authenticated credentials.
#' 
#' POST data to a URL using an HTTP POST request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKI package. If the PKI package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated POST
#' @param encode the type of encoding to use for the POST body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the POST request
#' @return the HTTP response from the request
auth_post <- function(url, encode="multipart", body=as.list(NA)) {
    cert <- NULL
    auth <- F
    response <- auth_put_post_delete("post", url, encode, body)
    return(response)
}

#' PUT a resource with authenticated credentials.
#' 
#' PUT data to a URL using an HTTP PUT request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKI package. If the PKI package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated PUT
#' @param encode the type of encoding to use for the PUT body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the PUT request
#' @return the HTTP response from the request
auth_put <- function(url, encode="multipart", body=as.list(NA)) {
    cert <- NULL
    auth <- F
    response <- auth_put_post_delete("put", url, encode, body)
    return(response)
}

#' DELETE a resource with authenticated credentials.
#' 
#' DELETE data at a URL using an HTTP DELETE request using authentication credentials 
#' provided in a client certificate.  Authenticated access depends on the suggested
#' PKI package. If the PKI package is not installed, then the request fails.
#' @param url The URL to be accessed via authenticated DELETE
#' @param encode the type of encoding to use for the DELETE body, defaults to 'multipart'
#' @param body a list of data to be included in the body of the DELETE request
#' @return the HTTP response from the request
auth_delete <- function(url, encode="multipart", body=as.list(NA)) {
    cert <- NULL
    auth <- F
    response <- auth_put_post_delete("delete", url, encode, body)
    return(response)
}

#' Determine if the PKI package is installed
check4PKI <- function() {
    if (!requireNamespace("PKIext", quietly = TRUE)) {
        invisible(FALSE)
    } else {
        invisible(TRUE)
    }
}

#' User agent string
#' 
#' Get a string representation of the user agent to be sent to the server along
#' with other request details.
get_user_agent <- function() {
    info <- sessionInfo()
    local_agent <- sprintf("dataone/%s curl/%s Rcurl/%s httr/%s", 
                                 info$otherPkgs$dataone$Version, 
                                 curlVersion()$version, 
                                 info$otherPkgs$RCurl$Version, 
                                 info$otherPkgs$httr$Version)
    return(local_agent)
}
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
#
### This file contains functions useful to the dataone package methods
###############################################################################



#' Encode the Input for Solr Queries
#' 
#' Treating all special characters and spaces as literals, backslash escape special
#' characters, and double-quote if necessary 
#' @param segment : a string to encode
#' @returnType character
#' @return the encoded form of the input
#' @examples encodeSolr("this & that")
#' 
#' @author rnahf
#' @export
encodeSolr <- function(segment) {
    inter <- gsub("([-+:?*~&^!|\"\\(\\)\\{\\}\\[\\]])","\\\\\\1",segment, perl=TRUE) 
    if (grepl(" ",inter)) {
        return(paste0("\"",inter,"\""))
    }
    return(inter)
}




#' Encode the Input for a URL Query Segment
#' 
#' Encodes the characters of the input so they are not interpretted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param querySegment : a string to encode
#' @returnType character
#' @return the encoded form of the input
#' @examples fullyEncodedQuery <- paste0("q=id:",encodeUrlQuery(encodeSolr("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5")))
#' @author rnahf
#' @export
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




#' Encode the Input for a URL Path Segment
#' 
#' Encodes the characters of the input so they are not interpretted as reserved
#' characters in url strings.  Will also encode non-ASCII unicode characters.
#' @param pathSegment : a string to encode
#' @returnType character
#' @return the encoded form of the input
#' @examples fullyEncodedPath <- paste0("cn/v1/object/",encodeUrlPath("doi:10.6085/AA/YBHX00_XXXITBDXMMR01_20040720.50.5"))
#' @author rnahf
#' @export
encodeUrlPath <- function(pathSegment) {
    encoded <- J("org/dataone/service/util/EncodingUtilities","encodeUrlPathSegment", pathSegment)
    if (!is.null(e<-.jgetEx())) {
        print("Java exception was raised")
        print(.jcheck(silent=FALSE))
    }
    return(encoded)
}

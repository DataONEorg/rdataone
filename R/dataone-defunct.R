#' @title Defunct
#' @description The following items are defunct in this release of dataone and are
#' no longer supported.
#' @name dataone-defunct
#' @keywords internal
#' @section These S4 methods are defunct:
#' \describe{
#'  \item{[D1Object()]}{: A representation of a DataObject}
#'  \describe{
#'    \item{[D1Object-initialize()]}{: Initialize a D1Object}
#'    \item{[getData()]}{: Get the data content of a specified D1Object.}
#'    \item{[getIdentifier()]}{: Get the identifier of the D1Object.}
#'    \item{[getFormatId()]}{: Get the formatId of the D1Object}
#'    \item{[setPublicAccess()]}{: Add a Rule to the AccessPolicy to make the object publicly readable.}
#'    \item{[canRead()]}{: Test whether the provided subject can read an object.}
#'    \item{[asDataFrame()]}{: Return the D1Object as a data.frame.}
#'    \item{[setObsoletedBy()]}{: Set a pid as being obsoleted by another pid.}
#'  } 
#' \item{[D1Client()]}{: The DataONE client class used to download, update and search for data in the DataONE network}
#'  \describe{
#'    \item{[d1SolrQuery()]}{: A method to query the DataONE solr endpoint of the Coordinating Node.}
#'    \item{[d1IdentifierSearch()]}{: Query the DataONE Solr endpoint of the Coordinating Node.}
#'    \item{[createDataPackage()]}{: Create a DataPackage on a DataONE Member Node}
#'    \item{[getMN()]}{: Get a member node client based on its node identifier.}
#'    \item{[convert.csv()]}{: Convert a DataFrame to Standard CSV.}
#'    \item{[addData()]}{: Add a D1Object containing a data object to a DataPackage}
#'    \item{[createD1Object()]}{: Create the Object in the DataONE System}
#'    \item{[getD1Object()]}{: Download a data object from the DataONE Federation.}
#'  }
#' \item{`EMLParser`}{: A representation of a DataObject}
#'  \describe{
#'    \item{[documented.entityNames()]}{: The entity names associated with each table are returned.}
#'    \item{[documented.d1Identifiers()]}{: Get the DataONE identifier associated with each table.}
#'    \item{[documented.sizes()]}{: Get the table size.}
#'  }
#' \item{`AbstractTableDescriber`}{: Base Class for Specific Metadata Parsers}
#'  \describe{
#'    \item{[data.formatFamily()]}{: Get the table format family.}
#'    \item{[data.tableFieldDelimiter()]}{: Get the table field delimiter.}
#'    \item{[data.tableQuoteCharacter()]}{: Get the table quote character.}
#'    \item{[data.characterEncoding()]}{: The character encoding used, for example "UTF-8"}
#'    \item{[data.tableAttributeOrientation()]}{: Which way to the attribute headers run?  Most data has a header row}
#'    \item{[data.tableSkipLinesHeader()]}{: The specified number of lines are skipped.}
#'    \item{[data.tableMissingValueCodes()]}{: the missing value codes are defined in the metadata document for}
#'    \item{[data.tableAttributeNames()]}{: The attribute names are defined in the metadata document for}
#'    \item{[data.tableAttributeTypes()]}{: The attributes' data types are defined in the metadata document for}
#'    \item{[data.tableAttributeStorageTypes()]}{: The attributes' data storage types are defined in the metadata document for}
#'  }
#' }
NULL

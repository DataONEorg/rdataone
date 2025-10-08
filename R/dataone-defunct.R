#' @title Defunct
#' @description The following items are defunct in this release of dataone and are
#' no longer supported.
#' @name dataone-defunct
#' @keywords internal
#' @section These S4 methods are defunct:
#' * [D1Object()]: A representation of a DataObject
#'    * `D1Object-initialize()`: Initialize a D1Object
#'    * `getData()`: Get the data content of a specified D1Object
#'    * `getIdentifier()`: Get the identifier of the D1Object
#'    * `getFormatId()`: Get the formatId of the D1Object
#'    * `setPublicAccess()`: Add a Rule to the AccessPolicy to make the object publicly readable
#'    * `canRead()`: Test whether the provided subject can read an object
#'    * `asDataFrame()`: Return the D1Object as a data.frame
#'    * `setObsoletedBy()`: Set a pid as being obsoleted by another pid
#' * [D1Client()]: The DataONE client class used to download, update and search for data in the DataONE network
#'    * [d1SolrQuery()]: A method to query the DataONE solr endpoint of the Coordinating Node
#'    * [d1IdentifierSearch()]: Query the DataONE Solr endpoint of the Coordinating Node
#'    * [createDataPackage()]: Create a DataPackage on a DataONE Member Node
#'    * [getMN()]: Get a member node client based on its node identifier
#'    * [convert.csv()]: Convert a DataFrame to Standard CSV
#'    * `addData()`: Add a D1Object containing a data object to a DataPackage
#'    * [createD1Object()]: Create the Object in the DataONE System
#'    * [getD1Object()]: Download a data object from the DataONE Federation
#' * `EMLParser`: A representation of a DataObject
#'    * [documented.entityNames()]: The entity names associated with each table are returned
#'    * [documented.d1Identifiers()]: Get the DataONE identifier associated with each table
#'    * [documented.sizes()]: Get the table size
#' * `AbstractTableDescriber`: Base Class for Specific Metadata Parsers
#'    * [data.formatFamily()]: Get the table format family
#'    * [data.tableFieldDelimiter()]: Get the table field delimiter
#'    * [data.tableQuoteCharacter()]: Get the table quote character
#'    * [data.characterEncoding()]: The character encoding used, for example "UTF-8"
#'    * [data.tableAttributeOrientation()]: Which way to the attribute headers run?  Most data has a header row
#'    * [data.tableSkipLinesHeader()]: The specified number of lines are skipped
#'    * [data.tableMissingValueCodes()]: the missing value codes are defined in the metadata document for
#'    * [data.tableAttributeNames()]: The attribute names are defined in the metadata document for
#'    * [data.tableAttributeTypes()]: The attributes' data types are defined in the metadata document for
#'    * [data.tableAttributeStorageTypes()]: The attributes' data storage types are defined in the metadata document for
NULL

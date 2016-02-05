#' @title Deprecated Classes, Methods
#' @description The following items are deprecated in this release of dataone and will be
#' marked as Defunct and removed in a future version.
#' @name dataone-deprecated
#' @keywords internal
#' @section These S4 classes and associated methods are deprecated:
#' \itemize{
#'  \item{\code{\link{D1Object-class}}}{: A representation of a DataObject}
#'  \itemize{
#'    \item{\code{\link[dataone]{D1Object-initialize}}}{: Initialize a D1Object}
#'    \item{\code{\link[dataone]{getData}}}{: Get the data content of a specified D1Object.}
#'    \item{\code{\link[dataone]{getIdentifier}}}{: Get the identifier of the D1Object.}
#'    \item{\code{\link[dataone]{getFormatId}}}{: Get the formatId of the D1Object}
#'    \item{\code{\link[dataone]{setPublicAccess}}}{: Add a Rule to the AccessPolicy to make the object publicly readable.}
#'    \item{\code{\link[dataone]{canRead}}}{: Test whether the provided subject can read an object.}
#'    \item{\code{\link[dataone]{asDataFrame}}}{: Return the D1Object as a data.frame.}
#'  }
#'  \item{\code{\link{CertificateManager-class}}}{: Provides methods to manager X.509 certificates.}
#'  \itemize{
#'    \item{\code{\link{CertificateManager}}}{: Create a CertificateManager object.}
#'    \item{\code{\link{getCertLocation}}}{: Get the file path on disk of the client certificate file.}
#'    \item{\code{\link{showClientSubject}}}{: Get DataONE Identity as Stored in the CILogon Certificate.}
#'    \item{\code{\link{isCertExpired}}}{: Determine if an X.509 certificate has expired.}
#'    \item{\code{\link{getCertExpires}}}{: Show the date and time when an X.509 certificate expires.}
#'    \item{\code{\link{downloadCert}}}{: Open the CILogon Certificate download page in the default browser.}
#'    \item{\code{\link{obscureCert}}}{: Obscure the CILogon Client Certificate.}
#'    \item{\code{\link{restoreCert}}}{: Restore the CILogon client certificate by renaming it to its original location}
#'  }
#' }
#'  
#' @section These additional methods are deprecated:
#' \itemize{
#'  \item{\code{\link{convert.csv}}}{: Get the checksum for the data object associated with the specified pid.}
#'  \item{\code{\link{getMNodeId}}}{: Get the checksum for the data object associated with the specified pid.}
#'  \item{\code{\link{getD1Object}}}{: Download a data object from the DataONE Federation.}
#'  \item{\code{\link{createDataPackage}}}{: Download a data object from the DataONE Federation.}
#'  \item{\code{\link{getMN}}}{: Get a member node client based on its node identifier.}
#'  \item{\code{\link{getCN}}}{: Get the coordinating node associated with this D1Client object.}
#' }
NULL
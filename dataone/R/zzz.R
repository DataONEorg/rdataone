#' @title Deprecated Classes, Methods
#' @description The following items are deprecated in this release of dataone and will be
#' marked as Defunct and removed in a future version.
#' @name dataone-deprecated
#' @keywords internal
#' @section These S4 classes and all associated methods are deprecated:
#' \itemize{
#'  \item{\code{\link{D1Object-class}}}{: A representation of a DataObject}
#'  \item{\code{\link{CertificateManager-class}}}{: Provides methods to manager X.509 certificates.}
#'  }
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
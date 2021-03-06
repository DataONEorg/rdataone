% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataone-package.R
\docType{package}
\name{dataone}
\alias{dataone}
\title{Search, download and upload data to the DataONE network.}
\description{
The R package \emph{dataone} provides read/write access to data and metadata from the 
\href{https://www.dataone.org/}{DataONE} network of 
Member Node data repositories. Member Nodes in DataONE are independent data repositories that have adopted the DataONE 
services for interoperability, making each of the repositories accessible to client tools such as the DataONE R Client 
using a standard interface. The DataONE R Client can be used to access data files and to write new data and metadata files 
to nodes in the DataONE network.
}
\section{Classes}{

\itemize{
 \item{\code{\link[=AuthenticationManager-class]{AuthenticationManager}}}{: AuthenticationManager provides methods to validate DataONE authentication.}
 \item{\code{\link[=CNode-class]{CNode}}}{: A CNode represents a DataONE Coordinating Node and can be used to access its services.}
 \item{\code{\link[=D1Client-class]{D1Client}}}{: The D1Client class contains methods that perform high level dataone tasks.}
 \item{\code{\link[=D1Node-class]{D1Node}}}{: A base class for CNode and MNode.}
 \item{\code{\link[=MNode-class]{MNode}}}{: MNode provides functions interacting with the a DataONE Member Node repository.}
}
}

\seealso{
A description of the \emph{dataone} R package is available with the command: \code{'vignette("dataone-overview")'}.
}
\author{
Matthew B. Jones (NCEAS) and Peter Slaughter (NCEAS)
}

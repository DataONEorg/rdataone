\name{d1.obscureCert}
\alias{d1.obscureCert}
\title{Obscure the CILogon Client Certificate}
\usage{
  d1.obscureCert()
}
\description{
  Obscures the x509 certificate that CILogon installs,
  effectively making future interactions with the DataONE
  services anonymous.  Note, when the client certificate is
  obscured, you will not be able to create objects to
  DataONE, or build D1Objects, which uses the certificate
  to fill out fields in the system metadata.
}
\note{
  \code{restoreCert} is this method's inverse operation
}
\author{
  rnahf
}


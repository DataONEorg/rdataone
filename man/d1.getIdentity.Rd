\name{d1.getIdentity}
\alias{d1.getIdentity}
\title{Get DataONE Identity as Stored in the CILogon Certificate}
\usage{
  d1.getIdentity()
}
\value{
  the DataONE Subject that is your client's identity
}
\description{
  Returns Your Identity according to DataONE (and CILogon).
  If the certificate is expired, the character string will
  be prefixed with "[EXPIRED]"
}
\author{
  rnahf
}


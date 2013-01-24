\name{certificate}
\alias{certificate}
\alias{CILogon}
\alias{CILogin}
\alias{identity}
\alias{d1.getIdentity}
\alias{d1.downloadCert}
\alias{d1.getCertExpires}
\alias{d1.isCertExpired}
\alias{d1.obscureCert}
\alias{d1.restoreCert}
\title{Client Identity in DataONE}
\description{
  The dataone R package provides several functions for managing your DataONE
  identity (authorization status). Understanding how your identity is managed is 
  important for working with DataONE, especially when you get unexpected results. 
  For example, depending your authorization status, searches may or may return
  only public records, or the full set of public and private records. Object and 
  package retrievals might fail if some or all of the objects being retrieved are
  private.  Creates and identifier reservations might fail because your authorization
  certificate is missing or expired.
  
  The set of functions listed here provide the means to understand and, if necessary,
  manipulate the state of your DataONE identity, although you should rarely need
  to use most of them.  Refer to details for information about certificates.
  
  \code{d1.getIdentity()} - returns the client's identity, which is the same value
  as what CILogon displays when you download a certificate.
  
  \code{d1.downloadCert()} - opens a window in your default browser, and navigates to the CILogon page. 
  
  \code{d1.getCertExpires()} - returns the expiration date of the current certificate
  
  \code{d1.isCertExpired()} - (self explanatory)
  
  \code{d1.obscureCert()} - mostly for troubleshooting, this function renames your client
  certificate so that future connections to DataONE are as public. 
  
  \code{d1.restoreCert()} - the reciprocal of \code{d1.obscureCert()}, renames the certificate 
  back to the original filename so that connections are established using the 
  certificate, not as public.  
}
\usage{
  d1.getIdentity()
  d1.downloadCert()
  d1.getCertExpires()
  d1.isCertExpired()
  d1.obscureCert()
  d1.restoreCert()
}
\details{
  DataONE identifies you using CILogon-provided x509 certificates. DataONE has 
  partnered with CILogon to provide a widely-accessible certificate issuing mechanism 
  that allows DataONE users to use existing trusted institutional and public accounts.
  
  CILogon recognizes many identity providers, including many universities and
  also Google, so most times users new to DataONE can get certificates using one
  of their existing accounts. For more information about the CILogon service, see 
  \url{http://www.cilogon.org/faq} 
  
  X509 Certificates differ from typical username-password login schemes in that
  certificates can be used by more than one application, which is very useful when
  using more than one DataONE-enabled application.  The certificates CILogon issues
  for DataONE are so-called "short-lived" certificates that currently expire 18 hours 
  from the time of issuing.  Typically you will want to download a fresh certificate
  the first time you interact with DataONE each day.
}
\author{Rob Nahf}
\examples{
  \dontrun{
  d1.getIdentity()
  d1.downloadCert()
  d1.getCertExpires()
  d1.isCertExpired()
  d1.obscureCert()
  d1.restoreCert()
  }
}
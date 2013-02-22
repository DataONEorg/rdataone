\name{CertificateManager-class}
\Rdversion{1.1}
\docType{class}
\alias{CertificateManager}
\alias{downloadCert}
\alias{getCertExpires}
\alias{isCertExpired}
\alias{obscureCert}
\alias{restoreCert}
\alias{showClientSubject}
\alias{CertificateManager-class}
\alias{downloadCert,CertificateManager-method}
\alias{getCertExpires,CertificateManager-method}
\alias{isCertExpired,CertificateManager-method}
\alias{obscureCert,CertificateManager-method}
\alias{restoreCert,CertificateManager-method}
\alias{showClientSubject,CertificateManager-method}

\title{Class \code{"CertificateManager"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
Certificate Manager organizes a set of methods to understand and, if necessary,
manipulate the state of your DataONE identity, as represented by your CILogon
client certificate.   

Understanding how your identity is managed is 
important for working with DataONE, especially to avoid unexpected results. 
For example, depending your authorization status, searches may or may return
only public records, or the full set of public and private records. Object and 
package retrievals might fail if some or all of the objects being retrieved are
private.  Creates and identifier reservations might fail because your authorization
certificate is missing or expired.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CertificateManager", ...)}
or \code{CertificateManager()}
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{jClientIdManager}:}{Object of class \code{"jclassName"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{downloadCert}{\code{signature(x = "CertificateManager")}: 
    open the CILogin Certificate download page in the default browser
    }
    \item{getCertExpires}{\code{signature(x = "CertificateManager")}: 
    Show the date and time when the client certificate expires 
    }
    \item{isCertExpired}{\code{signature(x = "CertificateManager")}: 
    Is the CILogon certificate expired? 
    }
    \item{obscureCert}{\code{signature(x = "CertificateManager")}: 
    Provided mostly for troubleshooting, this method renames the CILogon certificate, 
    so that future connections to DataONE are anonymous (as public). 
    Note, when the client certificate is obscured, you will not be able to create 
    objects to DataONE, or build D1Objects, which uses the certificate to fill out 
    fields in the system metadata it creates. 
    }
    \item{restoreCert}{\code{signature(x = "CertificateManager")}: 
    Restores a previously obscured certificate so that future interactions with 
    the DataONE services are established using the certificate.
    }
    \item{showClientSubject}{\code{signature(x = "CertificateManager")}: 
    Returns the client subject (identity) according to DataONE, as as contained 
    in the CILogon certificate.   This is the same value that CILogon displays when
    you download a certificate.  If the certificate is expired, the character string 
    will be prefixed with "[EXPIRED]"
    }
  }
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
\author{
Matthew Jones and Rob Nahf
}
\examples{
showClass("CertificateManager")
}
\keyword{classes}

\docType{methods}
\name{D1Client}
\alias{D1Client}
\title{Construct a D1Client, using default env ("PROD") and nodeid ("")}
\arguments{
  \item{env}{The label for the DataONE environment to be
  using ('PROD','STAGING','SANDBOX','DEV')}

  \item{mn_nodeid}{The node Id of the application's 'home'
  node.  Should be already registered to the corresponding
  'env'}

  \item{...}{(not yet used)}
}
\value{
  the D1Client object representing the DataONE environment

  the D1Client object representing the DataONE environment
}
\description{
  Construct a D1Client, using default env ("PROD") and
  nodeid ("")

  Pass in the environment to be used by this D1Client, plus
  the id of the member node to be used for primary
  interactions such as creates
}
\author{
  mbjones

  rnahf
}


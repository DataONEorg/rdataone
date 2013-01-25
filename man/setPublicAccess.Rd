\name{setPublicAccess}
\alias{setPublicAccess}
\title{Add a Rule to the AccessPolicy to Make the Object Publicly Readable}
\arguments{
  \item{x}{D1Object}

  \item{...}{(not yet used)}
}
\value{
  NULL
}
\description{
  To be called prior to creating the object in DataONE.
  When called before creating the object, adds a rule to
  the access policy that makes this object publicly
  readable.  If called after creation, it will only change
  the system metadata locally, and will not have any
  affect.
}
\author{
  rnahf
}


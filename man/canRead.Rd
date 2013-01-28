\name{canRead}
\alias{canRead}
\title{Test whether the provided subject can read the object}
\arguments{
  \item{x}{D1Client}

  \item{subject}{: character}

  \item{...}{(not yet used)}
}
\value{
  TRUE or FALSE
}
\description{
  Using the AccessPolicy, tests whether the subject has
  read permission for the object.  This method is meant
  work prior to submission, so uses only the AccessPolicy
  to determine who can read (Not the rightsHolder field,
  which always can read.)
}
\author{
  rnahf
}

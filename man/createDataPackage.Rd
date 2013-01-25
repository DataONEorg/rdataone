\name{createDataPackage}
\alias{createDataPackage}
\title{Create the Data Package in the DataONE System}
\arguments{
  \item{x}{: D1Client}

  \item{dataPackage}{: The DataPackage instance to be
  submitted to DataONE for creation.}

  \item{...}{(not yet used)}
}
\value{
  NULL
}
\description{
  Creates the D1Objects contained in the DataPackage by
  calling the createD1Object() on each of the members, as
  well as assembling the resourceMap object from the
  recorded relationships, and calling create() on it as
  well. Any objects in the data map that have a
  dataUploaded value are assumed to be pre-existing in the
  system, and skipped.
}
\details{
  The DataPackage describes the collection of data object
  and their associated metadata object, with the
  relationships and members serialized into a document
  stored under, and retrievable with, the packageId as it's
  own distinct object.

  Members are created serially, and most errors in creating
  one object will interrupt the create process for the
  whole, resulting in some members will getting created,
  and the remainder not.
}
\author{
  rnahf
}
\references{
  See d1_libclient_java documentation D1Client.create()
  \url{"http://dev-testing.dataone.org:8080/hudson/job/d1_libclient_java/ws/d1_libclient_java/target/site/apidocs/org/dataone/client/D1Client.html#create"}
}


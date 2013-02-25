\name{dataone-package}
\alias{dataone-package}
\alias{dataone}
\docType{package}
\title{
DataONE R Client
%% ~~ package title ~~
}
\description{
A package that provides read/write access to data and metadata from 
the DataONE network of Member Node data repositories.

}
\details{
\tabular{ll}{
Package: \tab dataone\cr
Version: \tab 1.0.0\cr
Date: \tab 2013-02-14\cr
Depends: \tab R (>= 2.9.2), rJava (>= 0.8-5), XML (>= 3.95-0.1), methods, dataonelibs\cr
Imports: \tab rJava, XML\cr
SystemRequirements: \tab Java (>= 1.6)\cr
License: \tab file LICENSE\cr
Packaged: \tab 2013-02-14 20:54:27 UTC; jones\cr
Repository: \tab CRAN\cr
Date/Publication: \tab 2013-02-14 20:49:45\cr
URL: \tab http://www.dataone.org/investigator-toolkit/dataone_r\cr
BugReports: \tab https://redmine.dataone.org\cr
Built: \tab R 2.15.1; ; 2013-02-15 21:42:52 UTC; unix\cr
}

Index:
\preformatted{
AbstractTableDescriber-class
                        Class '"AbstractTableDescriber"'
CertificateManager-class
                        Class '"CertificateManager"'
D1Client-class          Class '"D1Client"'
D1Object-class          Class '"D1Object"'
DataPackage-class       Class '"DataPackage"'
EMLParser-class         Class '"EMLParser"'
dataone-package         Package to access data and metadata in the
                        DataONE repository federation.
}
}
%%~~ An overview of how to use the package, including the most important ~~
%%~~ functions ~~
\section{Getting Started}{

The R Client addresses 5 broad functional areas: client setup, search, data retrieval,
data submission, and dataFrame interoperability.

The dataone package uses S4 classes and methods, so finding help for each of the 
dataone classes can be accomplished with the command:
\preformatted{
> class ? <theClass>
}
for example:
\preformatted{
> class ? D1Client
}

\subsection{1. Client Setup}{
Client setup includes setting your member node (where submissions will go), and 
setting up your client subject.  (You are identified to DataONE by your client subject).
Most interaction with the DataONE system is mediated by the D1Client class - retrievals,
searches, submissions.  The D1Client 'constructor' method builds a D1Client object 
configured to the chosen environment and membernode.

some examples:
\preformatted{
    > cli <- D1Client() # builds a client to the production environment

    > cli <- D1Client(,"urn:node:WERSDF")   # builds a client to the 
                                            # production environment and sets
                                            # the default member node

    > cli <- D1Client("DEV", "urn:node:UIYOP")  # sets the environment to DEV, 
                                                # and sets the default member node 
}

There are also some helper functions for managing your client subject.
\preformatted{
    > cm <- CertificateManager()
    > downloadCert(cm)    # opens the CILogon page in your default browser, 
                          # to assist in getting your client certificate 
                          # downloaded
    > getCertExpires(cm)  # displays the date-time that your current 
                          # certificate is valid until.
}
For documentation on all of the CertificateManager helper functions, type:
\preformatted{
    > class ? CertificateManager
}
}
\subsection{2. Data Search}{
DataONE coordinating nodes expose a SOLR query endpoint that can be queried against
to get information about stored objects.  Those familiar with SOLR queries can use
the D1Client methods \code{d1SolrQuery}
\preformatted{
    > results <- d1SolrQuery(cli, list(q="foo",fl="identifier,etc...")) 
}
to return solr results for their own interpretation.

For more streamlined searches to get just the identifiers:
\preformatted{
    > d1IdentifierSearch(cli, "q=foo")
}
returns a character vector of the identifiers of records found using the word 'foo' 
as a search term.

Future development based on initial feedback on these search methods is planned.
}

\subsection{3. Data Retrieval}{
Data retrieval from DataONE is accomplished using an object Identifier, obtained 
either from a data search within R, from ONEMercury, or even in a published paper.

Ideally, the data you want to retrieve has been submitted as part of a package,
and you will be retrieving the entire package.
  
A package in DataONE terminology is a set of individual objects (files, usually)
that relate to each other, and whose relationships to each other are defined in 
a separate object called a Resource Map.  To retrieve a data package, use the following:
\preformatted{
    > cli <- D1Client()
    > packageOfInterest <- getPackage(cli,"interestingStuff")
}
the object 'packageOfInterest' is of type "DataPackage", and once you have it, you 
can start looking at the individual objects that make it up:
\preformatted{
    > members <- getIdentifiers(packageOfInterest)
    > member1 <- getMember(packageOfInterest,members[1])
    > member1[0]
    "D1Object"
}
At this point, you will notice that the individual objects are represented locally
with the objects of class "D1Object".  What's in it?
\preformatted{
    > getData(member1)  # returns the content of member1
}

At times you may wish to retrieve an individual object directly, without retrieving
the entire package.  Use the following method, \code{getD1Object}.
\preformatted{
    > cli <- D1Client()
    > item <- getD1Object(cli,"interestingObject")
}
A list of methods used for working with DataPackages and their member D1Objects 
can be found with:
\preformatted{
    > methods ? DataPackage
    > methods ? D1Object
}
}
\subsection{4. Submitting Data}{

Data submission functionality is still in development.  We have released
a basic set of functions for the three major related activities: assembling the
package, attending to who will have access to it, and submitting to DataONE.
However, the functionality for the second two activities is limited to setting
public access to objects, and creating new data. Future releases will support 
content updates, archiving, and fine tuning access policies.

The best practice is to submit new data as part of a package containing the data, 
the metadata that describes it, and the ORE resource map that defines the relationship 
between the two (or more).  Typically, the scope of a package is 1 metadata 
object along with 1 or more data objects it documents.  DataONE does support 
packages with multiple metadata objects and their data as well.

The DataPackage class provides methods for assembling the data and metadata objects 
and defining the "documents / documented-by" relationships that get fed into the 
resource map.  All that is needed do is adding the members of the data package, 
and telling it which (metadata) members document which (data) members.  After that,
you submit the dataPackage. For example:
\preformatted{
    env.label <- "STAGING"
    mn.nodeid <- "urn:node:foo"
    d1.client <- D1Client(env.label, mn.nodeid)

    d1o.d1 <- new("D1Object", id.d1, table.1.data, data.formatID, mn.nodeid)
    d1o.d2 <- new("D1Object", id.d2, table.2.data, data.formatID, mn.nodeid)
    d1o.d3 <- new("D1Object", id.d3, table.3.data, data.formatID, mn.nodeid)
    d1o.md1 <- new("D1Object", id.md1, metadata, md.formatID, mn.nodeid)

    ## let's make these publicly readable
    setPublicAccess(d1o.d1)
    setPublicAccess(d1o.d2)
    setPublicAccess(d1o.d3)
    setPublicAccess(d1o.md1)

    data.package <- new("DataPackage",packageId=packageId)

    addData(data.package,d1o.d1)
    addData(data.package,d1o.d2)
    addData(data.package,d1o.d3)
    addData(data.package,d1o.md1)
    insertRelationship(data.package, id.md1, c(id.d1, id.d2, id.d3))

    create(d1.client, data.package)
}

You might have noticed that there is no mechanism to make the resourceMap itself 
a public object.  Currently, the DataPackage create method makes all of the 
resourceMaps it creates public (after it creates the resourceMap.)  This definitely 
needs to change.
}
\subsection{5. dataFrame Interoperability}{
Once you have access to dataone content in R, it would be nice to have that data
in a useful R data structure, specifically the ubiquitous dataFrame.  This package
provides functions to serialize dataFrames (to .csv), and convert them back into
dataFrames.  Serializing as csv makes the dataset almost universally useful for
others.  

A simple example:
\preformatted{
    ## serialize to csv
    ## you can run this one!
    library(dataone)
    ## let's load in a built-in dataset that's a dataFrame
    data(trees)
    cli <- D1Client("SANDBOX")
    serializedTreeData <- convert.csv(cli, trees)
    dataFormatId <- "text/csv"

    treeData <- new("D1Object","treesId", 
                    serializedTreeData, dataFormatId, "urn:node:mnDemo5")

    ## show the serialized form of the trees data
    getData(treeData)  

    ## should display the original trees dataFrame
    asDataFrame(treeData) 
}

Some metadata contains information on how the data is serialized and stored, and
can provide useful information on how to deserialize the stored content correctly.
The power of working with data packages, instead of the individual object is revealed
by this.

The dataone R client provides additional \code{asDataFrame} methods to allow the
associated metadata that contains content storage information to participate in
deserialization.

examples:
\preformatted{
    df <- asDataFrame(data.package, dataMember1.id)

    df <- asDataFrame(data.object, its.metadata)

    table.describer <- EMLParser(its.metadata)
    df <- asDataFrame(data.object, table.describer)

    df <- asDataFrame(data.object, sep="\\t", ...)

    df <- asDataFrame(data.object)
}

In the first example the dataPackage uses the resource map to associate the data 
object with its describing metadata, and send that along to the deserializer.  
The second example is useful where there's no data package to do the association, 
but it is known by some other means.  

Currently, the dataone package can only parse EML documents, but has the ability
to plug in other metadata format parsers as they become available, by way of the 
AbstractTableDescriber virtual class.
}
}
\author{
Matthew Jones, Rob Nahf

Maintainer: Matthew Jones <jones@nceas.ucsb.edu>
%% ~~ The author and/or maintainer of the package ~~
}
%% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%% ~~ the R documentation directory ~~
\keyword{ package }
%%\examples{
%% ~~ simple examples of the most important functions ~~
%% \dontrun{
%%    library(dataone)
%%    d1 <- D1Client("DEV")
%%    dataPack <-getDataPackage(d1,"someknownPackageID")
%%    d1object <-getMember(dataPack,"someKnownMemberID") 
%%  }
%%}

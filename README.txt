dataone
-------
A client for DataONE, written in R. Uses the Java DataONE client lib for
accessing DataONE services.

Dependencies
-------------
rJava

Installation Notes
------------------
We need rJava to bind to the Java client libraries, be sure to install from
version 0.8-5 or later because prior versions have a classpath bug
> install.packages("rJava",,"http://rforge.net/",type="source")

Build notes
-----------
1. Include Java jars and classes in d1_client_r/inst/java
2. use "R CMD INSTALL d1_client_r" to install in the local R env
3. use "R CMD check d1_client_r" to check package validity
4. use "R CMD build d1_client_r" to build a distribution

Once installed, the package can be run in R using:
$ R
> library(dataone)
Loading required package: rJava
> d1.get("knb:testid:201017503654464")
restURL: http://localhost:8080/knb/session/?username=uid=kepler,o=unaffiliated,dc=ecoinformatics,dc=org&password=kepler&qformat=xml&op=login
method: POST
restURL: http://localhost:8080/knb/object/knb:testid:201017503654464?sessionid=D5E407E1348AFAA75B003B286AB3FAF6
method: GET
[1] "x,y,z\n1,2,3"
>

Troubleshooting
---------------
1. Java version on Mac OS X
  -- even though my user default for Java was 1.6, at times rJava still uses 1.5
  -- even switching to 1.6 under root doesn't help
  -- thus, classes compiled under 1.6 sometimes won't run in rJava
  -- recompile with -source 1.5 -target 1.5 and things should work
  -- in recent versions of rJava, Java 1.6 code is working fine
  -- I wrote a utility method to determine the java version used in rJava,
     execute it as:
     > d1.javaversion()
     [1] "1.5.0_24"
     [1] "1.5.0_24-b02-357-9M3165"
2. The doc and man directories still contain only stub documentation


Getting Started  (from email on 12/12/12)
---------------
I think there's probably 4 primary functional areas the R Client addresses:

1. setting the environment and your member node (the one where you publish to)
The D1Client 'constructor' method builds a D1Client object configured to the chosen
environment and membernode.

It's also the class that retrieves objects from that environment with either 
getD1Object, or getPackage.

examples:

d1.client <- D1Client("STAGING","urn:node:WERSDF")

d1.client <- D1Client("DEV")  # set the environment to DEV, no member node set

d1.client <- D1Client()  # builds a client to the production environemnt


2. submitting data to the member node:
The best practice is to submit new data as part of a package containing the data, 
the metadata that describes it, and the ORE resource map that defines the relationship 
between the two (or more) to dataONE.  Typically, a scope of a package is 1 metadata 
object along with 1 or more data objects it documents, but it could contain multiple 
metadata objects and their data. 

The DataPackage class provides methods for assembling the data and metadata objects 
and defining the "documents / documented-by" relationships that get fed into the 
resource map.  All you do is add the members of the data package, and tell it which 
(metadata) members document which (data) members.  After that you submit, or 
"create" the dataPackage.

example:

env.label <- "STAGING"
mn.nodeid <- "urn:node:foo"
d1.client <- D1Client(env.label, mn.nodeid)

d1o.d1 <- new("D1Object", id.d1, table.1.data, data.formatID, mn.nodeid)
d1o.d2 <- new("D1Object", id.d2, table.2.data, data.formatID, mn.nodeid)
d1o.d3 <- new("D1Object", id.d3, table.3.data, data.formatID, mn.nodeid)
d1o.md1 <- new("D1Object", id.md1, metadata, md.formatID, mn.nodeid)

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

When you build a new D1Object, only a minimal system metadata object is created.  
Typically, you'd want to add an AccessPolicy before posting it to the member node. 
The method setPublicAccess is a convenience method for adding an accessRule to the 
system metadata making an object publicly readable. For more thorough accessPolicy 
setting, for now, you'd have to work directly with the wrapped Java D1Object, but 
setPublicAccess is probably good enough for demonstrations and test objects.

You might notice too, that there's no mechanism to make the resourceMap itself a 
public object.  Currently, the DataPackage create method makes all of the resourceMaps 
it creates public (after it creates the resourceMap.)  We need to design a way to 
set access for all members of a DataPackage. Alas, not there yet.


3. getting data / metadata from DataONE:

d1.client <- D1Client("STAGING")
d1o <- getD1Object(d1.client, pid)
content <- getData(d1o)

dp <- getPackage(d1.client, pid)
members <- getIdentifiers(dp)
d1o <- getMember(dp,members[1])
content <- getData(d1o)

the reason you don't need the member node set in D1Client is that the R Client 
methods consult CN resolve to get the nodes that hold the requested object. 

4. converting data objects into useful things in R, specifically dataFrames.
This is currently limited to converting csv-like files.

examples:

df <- asDataFrame(data.package, dataMember.id)

df <- asDataFrame(data.object, its.metadata)

table.describer <- EMLParser(its.metadata)
df <- asDataFrame(data.object, table.describer)

df <- asDataFrame(data.object, sep="\t", ...)

df <- asDataFrame(data.object)


4a. importing / registering DataTableDescribers - the things that read the metadata 
and pull out the elements that give information on how the dataTable should be parsed.  
The dataone package currently includes the DataTableDescriber for EML documents, but 
the intention is to split it out into it's own package.  Something simple is set up 
to allow other metadata providers to build their own metadata parsers, and for the 
dataone package to find them when they are loaded, but further work is definitely 
needed.

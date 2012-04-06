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
version 0.8-5 because prior versions have a classpath bug
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
2. The doc and man directories are still from the tutorial, need to be updated.

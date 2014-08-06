library(dataone)

# Login
cm <- CertificateManager()
downloadCert(cm)
getCertExpires(cm)

# Select a repository to use for writing the data; these may change, especially for
# testing environments, and can be checked by insepcting the node list for each environment
#mn_nodeid <- "urn:node:KNB"              # MN for PROD env
mn_nodeid <- "urn:node:mnDemo8"           # MN for DEV env
#mn_nodeid <- "urn:node:mnSandboxUCSB1"   # MN for SANDBOX env
#mn_nodeid <- "urn:node:mnTestKNB" # MN for testing

# Initialize a client to interact with DataONE
## Create a DataONE client
#cli <- D1Client("PROD", mn_nodeid)
cli <- D1Client("DEV", mn_nodeid)
#cli <- D1Client("SANDBOX", mn_nodeid)

## Create some ids.
cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
id.dat <- paste("r_test1_dat", cur_time, "1", sep=".")
id.dat2 <- "primaryData.1.1"
id.dat3 <- "primaryData.2.1"
id.mta <- paste("r_test1_mta", cur_time, "1", sep=".")
id.res <- paste("r_test1_res", cur_time, "1", sep=".")
id.act <- paste("r_test1_act", cur_time, "1", sep=".")
id.pkg <- paste("r_test1_pkg", cur_time, "1", sep=".")

## Create a data table, and write it to csv format
testdf <- data.frame(x=1:10,y=11:20)
head(testdf)
csvdata <- convert.csv(cli, testdf)
format.dat <- "text/csv"

## Create the script object
script <- paste(readLines("plot.R"), collapse = '')
format.script <- "text/plain"

## Create the result object
resultData <- data.frame(x=1:10,y=11:20)
head(resultData)
result <- convert.csv(cli, resultData)
format.res <- "text/csv"

# Create a metadata object
metadata <- paste(readLines("test-metadata.xml"), collapse = '')
format.mta <- "eml://ecoinformatics.org/eml-2.1.1"

## Build a D1Object for the table, and upload it to the MN
d1Object.dat <- new(Class="D1Object", id.dat, csvdata, format.dat, mn_nodeid)

## Build a D1Object for the result, and upload it to the MN
d1Object.res <- new(Class="D1Object", id.res, result, format.res, mn_nodeid)

## Build a D1Object for the script, and upload it to the MN
d1Object.act <- new(Class="D1Object", id.act, script, format.script, mn_nodeid)

## Build a D1Object for the metadata, and upload it to the MN
d1Object.mta <- new("D1Object", id.mta, metadata, format.mta, mn_nodeid)

# Set access control on the data object to be public
setPublicAccess(d1Object.dat)
if (canRead(d1Object.dat,"public")) {
  print("successfully set public access");
} else {
  print("FAIL: did not set public access");
}

# Set access control on the result object to be public
setPublicAccess(d1Object.res)
if (canRead(d1Object.res,"public")) {
  print("successfully set public access");
} else {
  print("FAIL: did not set public access");
}

# Set access control on the action object to be public
setPublicAccess(d1Object.act)
if (canRead(d1Object.act,"public")) {
  print("successfully set public access");
} else {
  print("FAIL: did not set public access");
}

setPublicAccess(d1Object.mta)
if (canRead(d1Object.mta,"public")) {
  print("successfully set public access");
} else {
  print("FAIL: did not set public access");
}

# Assemble our data package containing both metadata and data
data.package <- new("DataPackage", packageId=id.pkg)
addData(data.package,d1Object.dat)
addData(data.package,d1Object.res)
addData(data.package,d1Object.act)
addData(data.package,d1Object.mta)
insertRelationship(data.package, id.mta, c(id.dat, id.res, id.act))
insertRelationship(data.package, id.res, c(id.act), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#wasGeneratedBy")
insertRelationship(data.package, id.act, c(id.dat), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#used")
insertRelationship(data.package, id.dat, c(id.dat2, id.dat3), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#wasDerivedFrom")

# Now upload the whole package to the member node
createDataPackage(cli, data.package)
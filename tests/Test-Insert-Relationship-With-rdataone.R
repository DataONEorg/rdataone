library(dataone)

# Login
cm <- CertificateManager()
downloadCert(cm)
getCertExpires(cm)

# Select a repository to use for writing the data; these may change, especially for
# testing environments, and can be checked by insepcting the node list for each environment
#mn_nodeid <- "urn:node:KNB"              # MN for PROD env
#mn_nodeid <- "urn:node:mnDemo8"           # MN for DEV env
#mn_nodeid <- "urn:node:mnSandboxUCSB1"   # MN for SANDBOX env
mn_nodeid <- "urn:node:mnTestKNB" # MN for testing

# Initialize a client to interact with DataONE
## Create a DataONE client
#cli <- D1Client("PROD", mn_nodeid)
#cli <- D1Client("DEV", mn_nodeid)
#cli <- D1Client("SANDBOX", mn_nodeid)
cli <- D1Client("STAGING2", mn_nodeid)

## Create some ids.
cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
id.dat <- paste("r_test_primaryData", cur_time, "1", sep=".")
id.mta <- paste("r_test_mta", cur_time, "1", sep=".")
id.result <- paste("r_test_derivedData", cur_time, "1", sep=".")
id.program <- paste("r_test_program", cur_time, "1", sep=".")
id.pkg <- paste("r_test_pkg", cur_time, "1", sep=".")

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
format.result <- "text/csv"

# Create a metadata object
metadata <- paste(readLines("metadata.xml"), collapse = '')
metadata <- gsub("result_data.1.1", id.result, metadata)
metadata <- gsub("program.1.1", id.program, metadata)
metadata <- gsub("data.1.1", id.dat, metadata)
format.mta <- "eml://ecoinformatics.org/eml-2.1.1"

## Build a D1Object for the table, and upload it to the MN
d1Object.dat <- new(Class="D1Object", id.dat, csvdata, format.dat, mn_nodeid)

## Build a D1Object for the result, and upload it to the MN
#d1Object.result <- new(Class="D1Object", id.result, binary.str, format.figure, mn_nodeid)
d1Object.result <- new(Class="D1Object", id.result, result, format.result, mn_nodeid)

## Build a D1Object for the script, and upload it to the MN
d1Object.program <- new(Class="D1Object", id.program, script, format.script, mn_nodeid)

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
setPublicAccess(d1Object.result)
if (canRead(d1Object.result,"public")) {
  print("successfully set public access");
} else {
  print("FAIL: did not set public access");
}

# Set access control on the action object to be public
setPublicAccess(d1Object.program)
if (canRead(d1Object.program,"public")) {
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
addData(data.package,d1Object.result)
addData(data.package,d1Object.program)
addData(data.package,d1Object.mta)
insertRelationship(data.package, id.mta, c(id.dat, id.result, id.program))
insertRelationship(data.package, id.result, c(id.program), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#wasGeneratedBy")
insertRelationship(data.package, id.program, c(id.dat), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#used")
insertRelationship(data.package, id.result, c(id.dat), "http://www.w3.org/ns/prov", "http://www.w3.org/ns/prov#wasDerivedFrom")

# Now upload the whole package to the member node
createDataPackage(cli, data.package)

#resourceMapString <- data.package@jDataPackage$serializePackage()
#resourceMapString
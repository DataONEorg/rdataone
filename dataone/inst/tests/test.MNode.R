context("MNode tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("MNode constructors", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
	expect_that(mn@endpoint, matches(mn_uri))
})
test_that("MNode getCapabilities()", {
	library(dataone)
	library(XML)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
    xml <- getCapabilities(mn)
	val <- xmlName(xmlRoot(xml))
	expect_that(val, matches("node"))
	expect_that(mn@identifier, matches("urn:node"))
})
test_that("MNode get()", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
    pid <- "doi:10.5063/F1QN64NZ"
    xml <- get(mn, pid)
	cname <- class(xml)[1]
	expect_that(cname, matches("XML"))
    pid <- "solson.5.1"
    obj <- get(mn, pid)
	cname <- class(obj)[1]
	expect_that(cname, matches("data.frame"))
    cn <- CNode()
    knb <- getMNode(cn, "urn:node:KNB")
	pid <- "doi:10.5063/F1QN64NZ"
	xml <- get(mn, pid)
	cname <- class(xml)[1]
	expect_that(cname, matches("XML"))
})
test_that("MNode getSystemMetadata()", {
    library(dataone)
    cn <- CNode()
    mn <- getMNode(cn, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    xml <- getSystemMetadata(mn, pid)
    cname <- class(xml)[1]
    expect_that(cname, matches("XML"))
})
test_that("MNode generateIdentifier()", {
    library(dataone)
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnTestKNB")
    newid <- generateIdentifier(mn, "UUID")
    cname <- class(newid)
    expect_that(cname, matches("character"))
    expect_that(newid, matches("urn:uuid:"))
})
test_that("MNode describe()", {
  library(dataone)
  mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
  mn <- MNode(mn_uri)
  res <- describe(mn, "knb.473.1")
  expect_is(res, "list")
  expect_equal(res$`content-type`, "text/xml")
})
test_that("MNode create(), update(), archive(), and delete()", {
    library(dataone)
    library(digest)
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnTestKNB")
    newid <- generateIdentifier(mn, "UUID")
    cname <- class(newid)
    expect_that(cname, matches("character"))
    expect_that(newid, matches("urn:uuid:"))
    
    # Ensure the user is logged in before running the tests
    cm <- CertificateManager()
    user <- showClientSubject(cm)
    isExpired <- isCertExpired(cm)
    expect_that(user, matches("cilogon"))
    expect_that(isExpired, is_false())
    
    # Create a data object, and convert it to csv format
    testdf <- data.frame(x=1:10,y=11:20)
    csvfile <- paste(tempfile(), ".csv", sep="")
    write.csv(testdf, csvfile, row.names=FALSE)
    
    # Create SystemMetadata for the object
    format <- "text/csv"
    size <- file.info(csvfile)$size
    sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
    sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
    expect_that(sysmeta@checksum, equals(sha1))
    expect_that(sysmeta@submitter, equals(user))
    expect_that(sysmeta@rightsHolder, equals(user))
    expect_that(sysmeta@originMemberNode, equals(mn@identifier))
    expect_that(sysmeta@authoritativeMemberNode, equals(mn@identifier))
    
    # Upload the data to the MN using create(), checking for success and a returned identifier
    response <- create(mn, newid, csvfile, sysmeta)
    expect_that(xmlValue(xmlRoot(response)), matches(newid))
    
    # Update the object with a new version
    updateid <- generateIdentifier(mn, "UUID")
    testdf <- data.frame(x=1:20,y=11:30)
    csvfile <- paste(tempfile(), ".csv", sep="")
    write.csv(testdf, csvfile, row.names=FALSE)
    size <- file.info(csvfile)$size
    sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
    sysmeta@identifier <- updateid
    sysmeta@size <- size
    sysmeta@checksum <- sha1
    sysmeta@obsoletes <- newid
    response <- update(mn, newid, csvfile, updateid, sysmeta)
    expect_that(xmlValue(xmlRoot(response)), matches(updateid))
    updsysmeta <- getSystemMetadata(mn, updateid)
    expect_that(xmlValue(xmlRoot(updsysmeta)[["obsoletes"]]), matches(newid))
    
    # Archive the object
    response <- archive(mn, newid)
    expect_that(xmlValue(xmlRoot(response)), matches(newid))
    newsysmeta <- getSystemMetadata(mn, newid)
    expect_that(xmlValue(xmlRoot(newsysmeta)[["archived"]]), matches("true"))
    
    #TODO: delete the object
    #response <- delete(mn, newid)
    
})
context("MNode tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("MNode constructors", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
	mn <- MNode(mn_uri)
	expect_that(mn@endpoint, matches(mn_uri))
})
test_that("MNode getCapabilities()", {
	library(dataone)
	library(XML)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
	mn <- MNode(mn_uri)
    xml <- getCapabilities(mn)
	val <- xmlName(xmlRoot(xml))
	expect_that(val, matches("node"))
	expect_that(mn@identifier, matches("urn:node"))
})
test_that("MNode getObject(), getChecksum()", {
    library(dataone)
    mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
    mn <- MNode(mn_uri)
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- getObject(mn, pid)
    xml <- xmlParseDoc(rawToChar(bytes), asText=TRUE)
    cname <- class(xml)[1]
    expect_that(cname, matches("XML"))
    pid <- "solson.5.1"
    obj <- getObject(mn, pid)
    df <- read.csv(text=rawToChar(obj))
    cname <- class(df)[1]
    expect_that(cname, matches("data.frame"))
    cn <- CNode()
    knb <- getMNode(cn, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- getObject(mn, pid)
    xml <- xmlParseDoc(rawToChar(bytes), asText=TRUE)
    cname <- class(xml)[1]
    expect_that(cname, matches("XML"))
    chksum <- getChecksum(mn, pid)
    expect_that(chksum, is_a("character"))
    expect_false(is.null(chksum))
})
test_that("MNode getSystemMetadata()", {
    library(dataone)
    cn <- CNode()
    mn <- getMNode(cn, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    sysmeta <- getSystemMetadata(mn, pid)
    cname <- class(sysmeta)[1]
    expect_that(cname, matches("SystemMetadata"))
    expect_that(sysmeta@identifier, matches("doi:10.5063/F1QN64NZ"))
})

test_that("MNode generateIdentifier()", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    cn <- CNode("STAGING")
    mn <- getMNode(cn, "urn:node:mnStageUCSB2")  
    # Suppress openssl, cert missing warnings
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
    if(authValid) {
      if(dataone:::getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
    } else {
      skip("This test requires valid authentication.")
    }
})

test_that("MNode generateIdentifier() on API v1 node", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    # Currently this is a v1 node, so only X.509 certs work for authentication
    # so this test should find and use a cert if it is available.
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnDemo9")
    # Suppress openssl, cert missing warnings
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
    if(authValid) {
      if(dataone:::getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
    } else {
      skip("This test requires valid authentication.")
    }
})

test_that("MNode describeObject()", {
  library(dataone)
  mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
  mn <- MNode(mn_uri)
  res <- describeObject(mn, "knb.473.1")
  expect_is(res, "list")
  expect_equal(res$`content-type`, "text/xml")
})

test_that("MNode describeObject() with authentication", {
  # This test requires valid DataONE user authentication and writes to unstable development machines
  skip_on_cran()
  library(dataone)
  library(uuid)
  library(digest)
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress openssl, cert missing warnings
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
  if(authValid) {
    if(dataone:::getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    user <- dataone:::getAuthSubject(am, mn)
    newid <- generateIdentifier(mn, "UUID")
    cname <- class(newid)
    testdf <- data.frame(x=1:10,y=11:20)
    csvfile <- paste(tempfile(), ".csv", sep="")
    write.csv(testdf, csvfile, row.names=FALSE)
    # Create SystemMetadata for the object
    format <- "text/csv"
    size <- file.info(csvfile)$size
    sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
    # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
    sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha1,
                     originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
    # sysmeta <- addAccessRule(sysmeta, "public", "read")
    # Upload the data to the MN using createObject(), checking for success and a returned identifier
    # The object is not created with public read access so that we can test that an authenticated
    # describeObject, i.e. can't read the object unless you have read access, in this case via
    # being the rightsholder.
    createdId <- createObject(mn, newid, csvfile, sysmeta)
    expect_false(is.null(createdId))
    res <- describeObject(mn, newid)
    expect_is(res, "list")
    expect_equal(res$`content-type`, "text/csv")
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("MNode createObject(), updateObject(), archive()", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    library(digest)
    library(datapack)
    library(uuid)
    library(XML)
    #cn <- CNode("SANDBOX")
    #cn <- CNode("DEV2")
    cn <- CNode("STAGING")
    # mnDemo1 is api v1 on 20151208, but that could change
    # Use this v1 node to test with both a current token available
    # and a certificate.
    #mnId <- "urn:node:mnSandboxUCSB2"
    #mnId <- "urn:node:mnDevUCSB2"
    mnId <- "urn:node:mnStageUCSB2"
    mn <- getMNode(cn, mnId)
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
    if (authValid) {
      if(dataone:::getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      user <- dataone:::getAuthSubject(am, mn)
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
      expect_that(user, matches("cilogon|dataone|orcid"))
      # Create a data object, and convert it to csv format
      testdf <- data.frame(x=1:10,y=11:20)
      csvfile <- paste(tempfile(), ".csv", sep="")
      write.csv(testdf, csvfile, row.names=FALSE)
      
      # Create SystemMetadata for the object
      format <- "text/csv"
      size <- file.info(csvfile)$size
      sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
      # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
      seriesId <- UUIDgenerate()
      sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha1,
                     originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier, seriesId=seriesId)
      sysmeta <- addAccessRule(sysmeta, "public", "read")
      expect_that(sysmeta@checksum, equals(sha1))
      expect_that(sysmeta@originMemberNode, equals(mn@identifier))
      expect_that(sysmeta@authoritativeMemberNode, equals(mn@identifier))
      
      # Upload the data to the MN using createObject(), checking for success and a returned identifier
      createdId <- createObject(mn, newid, csvfile, sysmeta)
      expect_false(is.null(createdId))
      expect_match(createdId, newid)
      
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
      newId <- updateObject(mn, newid, csvfile, updateid, sysmeta)
      expect_false(is.null(newId))
      expect_match(newId, updateid)
      updsysmeta <- getSystemMetadata(mn, updateid)
      expect_that(class(updsysmeta)[1], matches("SystemMetadata"))
      expect_that(updsysmeta@obsoletes, matches(newid))
      
      # Now get the sysmeta using the seriesId, if supported
      if(mn@APIversion >= "v2") {
        headSysmeta <- getSystemMetadata(mn, seriesId)
        expect_that(class(headSysmeta)[1], matches("SystemMetadata"))
        expect_that(updsysmeta@identifier, matches(headSysmeta@identifier))
      }
      
      # Archive the object
      response <- archive(mn, newid)
      expect_that(response, matches(newid))
      newsysmeta <- getSystemMetadata(mn, newid)
      expect_that(class(newsysmeta)[1], matches("SystemMetadata"))
      expect_that(newsysmeta@archived, is_true())
    } else {
      skip("This test requires valid authentication.")
    }
})

test_that("MNode createObject() works for large files", {
    # This line has to be commented out to run this test.
    skip("Skip large file testing.")
    if (grepl("Darwin", Sys.info()['sysname'])) {
        skip("fallocate not available on Mac")
    }
    library(dataone)
    library(digest)
    library(httr)
    cn <- CNode("STAGING")
    mn <- getMNode(cn, "urn:node:mnStageUCSB2")
    newid <- generateIdentifier(mn, "UUID")
    cname <- class(newid)
    expect_that(cname, matches("character"))
    expect_that(newid, matches("urn:uuid:"))
    # Ensure the user is logged in before running the tests
    # Set 'user' to authentication subject, if available, so we will have permission to change this object
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
    if (authValid) {
      #if(getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      user <- dataone:::getAuthSubject(am, mn)
      # TODO: Create a large data object using fallocate through a system call (only on linux)
      csvfile <- 'testdata.csv'
      csvsize <- '4G'
      system(paste("fallocate -l", csvsize, csvfile))
      
      # On a mac, use truncate instead (slower, and produces a sparse file)
      #csvsize <- '4294967296'    # truncate needs the size arg in bytes
      #system(paste("truncate", csvfile, csvsize))
      
      # Create SystemMetadata for the object
      format <- "text/csv"
      size <- file.info(csvfile)$size
      sha1 <- digest(csvfile, algo="sha1", serialize=FALSE, file=TRUE)
      sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha1, originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
      sysmeta <- addAccessRule(sysmeta, "public", "read")
      expect_that(sysmeta@checksum, equals(sha1))
      expect_that(sysmeta@originMemberNode, equals(mn@identifier))
      expect_that(sysmeta@authoritativeMemberNode, equals(mn@identifier))
      
      # Upload the data to the MN using createObject(), checking for success and a returned identifier
      # Note: createObject() will ensure that sysmeta@submitter, sysmeta@rightsHolder are set
      createdId <- createObject(mn, newid, csvfile, sysmeta)
      expect_false(is.null(createdId))
      expect_that(createdId, matches(newid)) 
      
      # Remove the big data file we createObjectd locally
      unlink(csvfile)
    } else {
      skip("This test requires valid authentication.")
    }
})

test_that("MNode getPackage() works", {
  library(uuid)
  # This test can exceed the CRAN test running time limits
  skip_on_cran()
  cn <- CNode("PROD")
  mn <- getMNode(cn, "urn:node:KNB")
  resMapPid <- "resourceMap_lrw.3.5"
  am <- AuthenticationManager()
  suppressWarnings(authValid <- dataone:::isAuthValid(am, mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  
  suppressWarnings(bagitFile <- getPackage(mn, id=resMapPid))
  expect_true(!is.null(bagitFile))
  expect_true(file.exists(bagitFile))
  # Now check error handling
  # Can't be a valid pid because we just created the unique string.
  #notApid <- sprintf("urn:uuid:%s", UUIDgenerate())
  #err <- try(bagitFile <- getPackage(mn, id=notApid), silent=TRUE)
  #expect_that(class(err), matches("try-error"))
  
})

test_that("updateSystemMetadata() works",{
  # This test requires valid DataONE user authentication and writes to unstable development machines
  skip_on_cran()
  library(dataone)
  # Create a csv file for the data object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  mnId <- "urn:node:mnStageUCSB2"
  d1c <- D1Client("STAGING", mnId)
  am <- AuthenticationManager()
  suppressWarnings(authValid <- dataone:::isAuthValid(am, d1c@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1c@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    subject <- dataone:::getAuthSubject(am, d1c@mn)
    do1 <- new("DataObject", format="text/csv", user=subject, mnNodeId=mnId, filename=csvfile)
    # Set replication off, to prevent the bug of serialNumber increasing due to replication bug
    uploadDataObject(d1c, do1, replicate=FALSE, public=TRUE)
    id1 <- getIdentifier(do1)
    md1 <- getSystemMetadata(d1c@mn, id1)
    expect_false(is.null(md1))
    id <- "uid=jsmith,o=NCEAS,dc=ecoinformatics,dc=org"
    md1 <- addAccessRule(md1, id, "read")
    expect_true(updateSystemMetadata(d1c@mn, md1@identifier, md1))
    md1New  <- getSystemMetadata(d1c@mn, id1)
    # Did the access policy get updated?
    ap <- md1New@accessPolicy
    expect_true(ap[ap$subject==id,"subject"] == id)
  } else {
      skip("This test requires valid authentication.")
  }
})

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
test_that("MNode get(), getChecksum()", {
    library(dataone)
    mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
    mn <- MNode(mn_uri)
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- get(mn, pid)
    xml <- xmlParseDoc(rawToChar(bytes), asText=TRUE)
    cname <- class(xml)[1]
    expect_that(cname, matches("XML"))
    pid <- "solson.5.1"
    obj <- get(mn, pid)
    df <- read.csv(text=rawToChar(obj))
    cname <- class(df)[1]
    expect_that(cname, matches("data.frame"))
    cn <- CNode()
    knb <- getMNode(cn, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- get(mn, pid)
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
    # Skip as this test requires authentication
    skip_on_cran()
    library(dataone)
    cn <- CNode("STAGING")
    mn <- getMNode(cn, "urn:node:mnStageUCSB2")  
    # Suppress PKIplus, cert missing warnings
    am <- AuthenticationManager()
    warnLevel <- getOption("warn")
    options(warn = -1)
    authValid <- isAuthValid(am, mn)
    options(warn = warnLevel)
    if(authValid) {
      if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
    }
})

test_that("MNode generateIdentifier() on API v1 node", {
    # Skip as this test requires authentication
    skip_on_cran()
    library(dataone)
    # Currently this is a v1 node, so only X.509 certs work for authentication
    # so this test should find and use a cert if it is available.
    cn <- CNode("STAGING2")
    mn <- getMNode(cn, "urn:node:mnDemo9")
    # Suppress PKIplus, cert missing warnings
    am <- AuthenticationManager()
    warnLevel <- getOption("warn")
    options(warn = -1)
    authValid <- isAuthValid(am, mn)
    options(warn = warnLevel)
    if(authValid) {
      if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
    } else {
      skip("This test requires valid authentication.")
    }
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
    skip_on_cran()
    library(dataone)
    library(digest)
    library(datapackage)
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
    # Suppress PKIplus, cert missing warnings
    warnLevel <- getOption("warn")
    options(warn = -1)
    authValid <- isAuthValid(am, mn)
    options(warn = warnLevel)
    if (authValid) {
      if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
      newid <- generateIdentifier(mn, "UUID")
      cname <- class(newid)
      expect_that(cname, matches("character"))
      expect_that(newid, matches("urn:uuid:"))
      
      # Set 'user' to authentication subject, if available, so we will have permission to change this object
      user <- getAuthSubject(am)
      # If subject isn't available from the current authentication method, then try
      # R options
      if (is.na(user) || user == "public") {
        user <- getOption("subject_dn")
        if(is.null(user) || is.na(user)) skip("This test requires that you set options(subject_dn = \"<your identity>\")")
      }
      
      expect_that(user, matches("cilogon|dataone"))
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
      sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1,
                     originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier, seriesId=seriesId)
      sysmeta <- addAccessRule(sysmeta, "public", "read")
      expect_that(sysmeta@checksum, equals(sha1))
      expect_that(sysmeta@submitter, equals(user))
      expect_that(sysmeta@rightsHolder, equals(user))
      expect_that(sysmeta@originMemberNode, equals(mn@identifier))
      expect_that(sysmeta@authoritativeMemberNode, equals(mn@identifier))
      
      # Upload the data to the MN using create(), checking for success and a returned identifier
      response <- create(mn, newid, csvfile, sysmeta)
      expect_that(response, not(is_null()))
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
    }
})

test_that("MNode create() works for large files", {
    skip("Skip large file testing.")
    skip_on_cran()
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
    # Suppress PKIplus, cert missing warnings
    warnLevel <- getOption("warn")
    options(warn = -1)
    authValid <- isAuthValid(am, mn)
    options(warn = warnLevel)
    if (authValid) {
      if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
      # Set 'subject' to authentication subject, if available, so we will have permission to change this object
      subject <- getAuthSubject(am)
      # If subject isn't available from the current authentication method, then try
      # the R options.
      if (is.na(subject) || user == "public") {
        subject <- getOption("subject_dn")
        if(is.null(subject) || is.na(subject)) skip("This test requires that you set options(subject_dn = \"<your identity>\")")
      }
      
      expect_that(subject, matches("cilogon|dataone"))
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
      sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, submitter=user, rightsHolder=user, checksum=sha1, originMemberNode=mn@identifier, authoritativeMemberNode=mn@identifier)
      sysmeta <- addAccessRule(sysmeta, "public", "read")
      expect_that(sysmeta@checksum, equals(sha1))
      expect_that(sysmeta@submitter, equals(user))
      expect_that(sysmeta@rightsHolder, equals(user))
      expect_that(sysmeta@originMemberNode, equals(mn@identifier))
      expect_that(sysmeta@authoritativeMemberNode, equals(mn@identifier))
      
      # Upload the data to the MN using create(), checking for success and a returned identifier
      response <- create(mn, newid, csvfile, sysmeta)
      expect_that(response, not(is_null()))
      expect_that(xmlValue(xmlRoot(response)), matches(newid)) 
      
      # Remove the big data file we created locally
      unlink(csvfile)
    }
})

test_that("MNode getPackage() works", {
  library(uuid)
  skip_on_cran()
  cn <- CNode("SANDBOX2")
  mn <- getMNode(cn, "urn:node:mnDemo2")
  resMapPid <- "urn:uuid:62febde3-5e7b-47b8-97a9-a874ffc9a180"
  #resMapPid <- "resourceMap_hpackage-test.1.1"
  am <- AuthenticationManager()
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, mn)
  options(warn = warnLevel)
  if (authValid) {
    if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
  }
  
  bagitFile <- getPackage(mn, id=resMapPid)
  expect_true(!is.null(bagitFile))
  expect_true(file.exists(bagitFile))
  # Now check error handling
  # Can't be a valid pid because we just created the unique string.
  #notApid <- sprintf("urn:uuid:%s", UUIDgenerate())
  #err <- try(bagitFile <- getPackage(mn, id=notApid), silent=TRUE)
  #expect_that(class(err), matches("try-error"))
  
})

test_that("updateSystemMetadata() works",{
  skip_on_cran()
  library(dataone)
  # Create a csv file for the data object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  mnId <- "urn:node:mnStageUCSB2"
  d1c <- D1Client(env="STAGING", mNodeid=mnId)
  
  am <- AuthenticationManager()
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, d1c@mn)
  options(warn = warnLevel)
  if (authValid) {
    if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authenticatin w/cert on Mac OS X")
    # Set 'user' to authentication subject, if available, so we will have permission to change this object
    subject <- getAuthSubject(am)
    # If subject isn't available from the current authentication method, then try
    # R options
    if (is.na(subject) || subject == "public") {
      subject <- getOption("subject_dn")
      if(is.null(subject) || is.na(subject)) skip("This test requires that you set options(subject_dn = \"<your identity>\")")
    }
    
    do1 <- new("DataObject", format="text/csv", user=subject, mnNodeId=mnId, filename=csvfile)
    # Set replication off, to prevent the bug of serialNumber increasing due to replication bug
    uploadDataObject(d1c, do1, replicate=FALSE, public=TRUE)
    id1 <- getIdentifier(do1)
    md1 <- getSystemMetadata(d1c@mn, id1)
    expect_false(is.null(md1))
    id <- "uid=slaughter,o=NCEAS,dc=ecoinformatics,dc=org"
    md1 <- addAccessRule(md1, id, "read")
    expect_true(updateSystemMetadata(d1c@mn, md1@identifier, md1))
    md1New  <- getSystemMetadata(d1c@mn, id1)
    # Did the access policy get updated?
    ap <- md1New@accessPolicy
    expect_true(ap[ap$subject==id,"subject"] == id)
  }
})

context("CNode tests")
test_that("dataone library loads", {
	expect_true(require(dataone))
})
test_that("CNode constructors", {
	library(dataone)
    # If not specified, "PROD" environment is used.
	#cn <- CNode()
	expect_match(cnProd@endpoint, "https://cn.dataone.org/cn")
	#cn <- CNode("PROD")
	expect_match(cnProd@endpoint, "https://cn.dataone.org/cn")
	# Skip unstable test environments.
	skip_on_cran()
	cn <- CNode("STAGING2")
	expect_match(cn@endpoint, "https://cn-stage-2.test.dataone.org/cn")
	#cn <- CNode("DEV")
	#expect_match(cn@endpoint, "https://cn-dev.test.dataone.org/cn")
})
test_that("CNode listNodes()", {
  library(dataone)
  #cn <- CNode("PROD")
  nodelist <- listNodes(cnProd)
  expect_that(length(nodelist) > 0, is_true())
  expect_match(class(nodelist[[1]]), "Node")
  expect_match(nodelist[[1]]@identifier, "urn:node:")
  expect_match(nodelist[[1]]@type, "cn|mn")
  expect_match(nodelist[[1]]@state, "up")
  expect_match(nodelist[[length(nodelist)]]@identifier, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@baseURL, "http")
  expect_match(nodelist[[length(nodelist)]]@subject, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@type, "cn|mn")
})

test_that("CNode getObject()", {
  library(dataone)
  library(XML)
  #cn <- CNode("PROD")
  pid <- "aceasdata.3.2"
  obj <- getObject(cnProd, pid)
  xml <- xmlParseDoc(rawToChar(obj), asText=TRUE)
  cname <- class(xml)[1]
  expect_match(cname, "XML")
  chksum <- getChecksum(cnProd, pid)
  expect_that(chksum, is_a("character"))
  expect_false(is.null(chksum))
})

test_that("CNode getSystemMetadata()", {
  library(dataone)
  #cn <- CNode("PROD")
  pid <- "aceasdata.3.2"
  sysmeta <- getSystemMetadata(cnProd, pid)
  expect_match(sysmeta@identifier, pid)
})

test_that("CNode describeObject()", {
  library(dataone)
  #cn <- CNode("PROD")
  pid <- "aceasdata.3.2"
  res <- dataone::describeObject(cnProd, pid)
  expect_is(res, "list")
  expect_match(res$`content-type`, "text/xml")
})

test_that("CNode getMNode()", {
  library(dataone)
  skip_on_cran()
  #cn <- CNode("PROD")
  nodelist <- listNodes(cnProd)
  nodeid <- nodelist[[length(nodelist)]]@identifier
  newnode <- getMNode(cnProd, nodeid)
  expect_match(class(newnode), "Node")
  expect_match(newnode@identifier, "urn:node:")
  expect_match(newnode@type, "cn|mn")
  expect_match(newnode@baseURL, "http")
  expect_match(newnode@subject, "urn:node:")
  suppressWarnings(newnode <- getMNode(cnProd, "NOT_A_NODE_ID"))
  expect_that(newnode, is_a("NULL"))
})

test_that("CNode resolve()",{
  library(dataone) 
  #cn <- CNode("PROD")
  id <- "0d7d8e0e-93f5-40ab-9916-501d7cf93e15"
  res <- resolve(cnProd,id)
  expect_match(res$id, id)
  expect_match(typeof(res$data),"list")
})

test_that("CNode reserveIdentifier(), hasReservation() works",{
  # Skip this test that is dependant on unstable test environments
  skip_on_cran()
  library(dataone)
  library(uuid)
  cn <- CNode("STAGING2")
   
  # For hasReservation(), we have to use the same subject that is in the authorization token or X.509 certificate.
  # Until the dataone package can decrypt auth tokens, we have to manually provide same subject
  # used by reserveIdentifier.  
  am <- AuthenticationManager()
  # Suppress openssl, cert missing warnings
  suppressMessages(authValid <- dataone:::isAuthValid(am, cn))
  # First check if authentication is available and if not, skip this test
  if (authValid) {
    # TODO: remove this check when Mac OS X can be used with certificates
    if(dataone:::getAuthMethod(am, cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Set 'subject' to authentication subject, if available, so this userId can check a reservation that it made
    subject <- dataone:::getAuthSubject(am, cn)
    myId <- sprintf("urn:uuid:%s", UUIDgenerate())
    # researveIdentifier will create the reservation using only the client subject from
    # the current authentication method - either auth token or certificate. 
    newId <- reserveIdentifier(cn, myId)
    expect_match(myId, newId)
    # Have to specify the subject for hasReservation
    hasRes <- hasReservation(cn, newId, subject=subject)
    expect_true(hasRes, info=sprintf("Didn't find reserved identifier %s", myId))
  } else {
      skip("This test requires valid authentication.")
  }
})

test_that("CNode listFormats, getFormat",{
  skip_on_cran()
  library(dataone) 
  #cn <- CNode("PROD")
  fmts <- listFormats(cnProd)
  expect_that(is.data.frame(fmts),is_true())
  expect_gt(length(grep("eml", fmts$ID)), 0)
  # CHeck that the name returned by getFormat matches the name
  # requested, and in the data.frame from listFormats
  for (i in 1:length(fmts)) {
    thisFmt <- getFormat(cnProd, fmts[i,'ID'])
    expect_match(fmts[i,'Name'], thisFmt$name)
  }
})

test_that("AuthenticationManager isAuthValid() for v2 node works", {
  skip_on_cran()
  library(dataone)
  am <- AuthenticationManager()
  expect_false(is.null(am))
  # v2 node
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress openssl, cert missing warnings
  authValid <- dataone:::isAuthValid(am, mn)
  if(!authValid) {
    expect_match(dataone:::getAuthSubject(am, mn), "public")
  } else {
    expect_match(dataone:::getAuthMethod(am, mn), "cert|token")
    expect_false(dataone:::isAuthExpired(am, mn))
  }
})

test_that("AuthenticationManager getAuthMethod(), getToken(), getCert() work", {
  skip_on_cran()
  am <- AuthenticationManager()
  expect_false(is.null(cm))
  expect_false(is.null(am))
  skip_on_cran()
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress openssl, cert missing warnings
  suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
  if(authValid) {
    expect_false(is.na(dataone:::getAuthMethod(am, mn)))
    if(dataone:::getAuthMethod(am, mn) == "token") {
      expect_false(is.na(dataone:::getToken(am, mn)))
    } else {
      expect_false(is.na(dataone:::getCert(am)))
      expect_true(file.exists(dataone:::getCert(am)))
    }
  }
})

test_that("getAuthExpires() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  expect_false(is.null(cm))
  expect_false(is.null(am))
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress openssl, cert missing warnings
  suppressMessages(authValid <- dataone:::isAuthValid(am, mn))
  if(authValid) {
    expect_false(is.na(dataone:::getAuthMethod(am, mn)))
    # TODO: Check auth token expiration when R JWT package is available.
    if(dataone:::getAuthMethod(am, mn) == "token") {
      expect_false(is.na(dataone:::getToken(am, mn)))
    } else {
      expect_false(is.na(dataone:::getCert(am)))
      expect_false(dataone:::isAuthExpired(am, mn))
      expect_false(is.na(dataone:::getAuthExpires(am, mn)))
    }
  }
})

test_that("isCertExpired() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("STAGING")
  # Suppress openssl, cert missing warnings
  suppressMessages(authValid <- dataone:::isAuthValid(am, cn))
  if(authValid) {
    expDate <- dataone:::getAuthExpires(am, cn)
    # TODO: check token exp date when JWT available.
    #ct <- format(Sys.time(), format="%F %H:%M:%S GMT", tz="UTC")
    ct <- as.POSIXct(Sys.time(), tz="GMT") 
    if(!is.na(expDate) && !is.null(expDate)) {
      if(dataone:::isAuthExpired(am, cn)) {
        expect_true(expDate > ct) 
      } else {
        expect_false(expDate < ct) 
      }
    }
  }
})

test_that("getAuthSubject() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("STAGING")
  # Suppress openssl, cert missing warnings
  authValid <- dataone:::isAuthValid(am, cn)
  result <- dataone:::getAuthSubject(am, cn)
  if (!authValid) {
    # No valid authentication, so should return "public" user
    expect_match(result, "public")
  } else {
    # Testing normal case
    expect_true(length(result) > 0)
  }
})

test_that("obscureAuth(), restoreAuth() work", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("STAGING")
  # Disable authentication
  # Suppress openssl, cert missing warnings
  am <- dataone:::obscureAuth(am)
  expect_false(dataone:::isAuthValid(am, cn))
  expect_equal(dataone:::getAuthSubject(am, cn), "public")
  expect_equal(dataone:::getToken(am, cn), as.character(NA))
  expect_equal(dataone:::getCert(am), as.character(NA))
  am <- restoreAuth(am)
  # Suppress openssl, cert missing warnings
  authValid <- dataone:::isAuthValid(am, cn)
  if(authValid) {
    if(dataone:::getAuthMethod(am, cn) == "token") {
      expect_false(identical(getToken(am, cn), as.character(NA)))
    } else {
      expect_false(identical(getCert(am), as.character(NA)))
    }
  }
})

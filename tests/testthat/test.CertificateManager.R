context("CertificateManager tests")

test_that("CertificateManager getCertLocation()", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  suppressWarnings(cm <- CertificateManager())
  expect_false(is.null(cm))
  suppressWarnings(location <- getCertLocation(cm))
  # No cert found so skip rest of test
  if(!is.null(location)) {
    cname <- class(location)
    expect_match(cname, "character", info="Tests require a valid X509 certificate")
    expect_match(location, "x509", info="Tests require a valid X509 certificate")
  }
})

test_that("CertificateManager loads", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  suppressWarnings(cm <- CertificateManager())
  expect_false(is.null(cm))
})

test_that("getCertExpires", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    suppressWarnings(cm <- CertificateManager())
    suppressWarnings(expires <- getCertExpires(cm))
    if (is.null(expires)) {
        ## if no certificate is installed, this is the correct answer
        expect_that(expires, equals(NULL))
    } else {
        ## need to see a date string
      expect_true(expires > "1901-01-01 00:00:00")
    }
})

test_that("isCertExpired", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    suppressWarnings(cm <- CertificateManager())
    suppressWarnings(isExpired <- isCertExpired(cm))
    # TODO: determine why getCertExpires doesn't return expiration for expired certs
    # BTW: isCertExpires works correctly for expired certs.
    #if (is.null(getCertExpires(cm))) {
    #    ## if no certificate is installed, then should equal TRUE
    #    expect_that(isExpired, expect_true())
    #} else {
    #    ## if a valid certificate is installed, then it should be FALSE
    ##    expect_that(isExpired, expect_false())
    #}
})

test_that("showClientSubject", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    suppressWarnings(cm <- CertificateManager())
    suppressWarnings(result <- showClientSubject(cm))
    suppressWarnings(expires <- getCertExpires(cm))
    if (is.null(expires)) {
        # Testing no certificate case
        expect_match(result, "public")
    } else if (suppressWarnings(isCertExpired(cm))) {
        # Testing expired certificate case
        expect_match(result, "public")
    } else {
        # Testing normal case
        expect_that(length(result) > 0, expect_true())
    }
})

test_that("obscureCert and restoreCert", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    suppressWarnings(cm <- CertificateManager())
    suppressWarnings(subject1 <- showClientSubject(cm))
    if (!subject1 == "public") {
      suppressWarnings(cm <- obscureCert(cm))
      suppressWarnings(subject2 <- showClientSubject(cm))
      expect_match(subject2, "public")
      suppressWarnings(restoreCert(cm))
      suppressWarnings(subject3 <- showClientSubject(cm))
      expect_match(subject3, subject1)
    }
})

test_that("custom certificate location", {
  skip_on_cran()
  if(!suppressWarnings(require("openssl", quietly=TRUE))) skip("This test requires the openssl package")
  #if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    suppressWarnings(cm <- CertificateManager())
    suppressWarnings(subject1 <- showClientSubject(cm))
    if (subject1 == "public") {
      suppressWarnings(cm <- obscureCert(cm))
      expect_true(is.na(cm@location))
      suppressWarnings(cm <- restoreCert(cm))
    } else {
        suppressWarnings(cert <- getCertLocation(cm))
        custom_cert <- paste0(tempfile(), ".x509")
        file.copy(cert, custom_cert)
        cm@location <- custom_cert
        suppressWarnings(subject2 <- showClientSubject(cm))
        expect_match(subject2, subject1)
        suppressWarnings(newCertLoc <- getCertLocation(cm))
        expect_equal(custom_cert, newCertLoc)
    }
})
context("CertificateManager tests")

test_that("CertificateManager getCertLocation()", {
  skip_on_cran()
    cm <- CertificateManager()
    expect_that(is.null(cm), is_false())
    location <- getCertLocation(cm)
    cname <- class(location)
    expect_that(cname, matches("character"), info="Tests require a valid X509 certificate")
    expect_that(location, matches("x509"), info="Tests require a valid X509 certificate")
})

test_that("CertificateManager loads", {
  cm <- CertificateManager()
  expect_that(is.null(cm), is_false())
})

test_that("getCertExpires", {
    cm <- CertificateManager()
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        ## if no certificate is installed, this is the correct answer
        expect_that(expires, equals(NULL))
    } else {
        ## need to see a date string
    }
})

test_that("isCertExpired", {
    cm <- CertificateManager()
    isExpired <- isCertExpired(cm)
    if (is.null(getCertExpires(cm))) {
        ## if no certificate is installed, then should equal TRUE
        expect_that(isExpired, is_true())
    } else {
        ## if a valid certificate is installed, then it should be FALSE
        expect_that(isExpired, is_false())
    }
})

test_that("showClientSubject", {
    cm <- CertificateManager()
    result <- showClientSubject(cm)
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        # Testing no certificate case
        expect_that(result, matches("public"))
    } else if (isCertExpired(cm)) {
        # Testing expired certificate case
        expect_that(result, matches("public"))
    } else {
        # Testing normal case
        expect_that(length(result) > 0, is_true())
    }
})

test_that("obscureCert and restoreCert", {
  skip_on_cran()
    cm <- CertificateManager()
    subject1 <- showClientSubject(cm)
    if (subject1 == "public") {
        expect_that(obscureCert(cm), throws_error())
    } else {
        cm <- obscureCert(cm)
        subject2 <- showClientSubject(cm)
        expect_that(subject2, matches("public"))
        restoreCert(cm)
        subject3 <- showClientSubject(cm)
        expect_that(subject3, matches(subject1))
    }
})

test_that("custom certificate location", {
    cm <- CertificateManager()
    subject1 <- showClientSubject(cm)
    if (subject1 == "public") {
      cm <- obscureCert(cm)
        expect_true(is.na(cm@location))
    } else {
        cert <- getCertLocation(cm)
        custom_cert <- paste0(tempfile(), ".x509")
        file.copy(cert, custom_cert)
        cm@location <- custom_cert
        subject2 <- showClientSubject(cm)
        expect_that(subject2, matches(subject1))
        expect_that(custom_cert, matches(getCertLocation(cm)))
    }
})
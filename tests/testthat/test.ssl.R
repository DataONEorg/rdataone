test_that("curl SSL library expected", {
    skip_on_cran()
    ssl_version <- curl::curl_version()$ssl_version
    expect_match(ssl_version, "OpenSSL|LibreSSL|Schannel|GnuTLS")
})

test_that("CNode constructors work in TLSv1.3 sandbox server", {
    skip_on_cran()
    library(dataone)
    cnSandbox <- CNode("SANDBOX")
    expect_match(cnSandbox@endpoint, "https://cn-sandbox.test.dataone.org/cn")
})

test_that("CNode listNodes() works in TLSv1.3 sandbox server", {
    skip_on_cran()
    library(dataone)
    cnSandbox <- CNode("SANDBOX")
    nodelist <- listNodes(cnSandbox)
    expect_true(length(nodelist) > 0)
})

test_that("D1Client constructors work in TLSv1.3 sandbox server", {
    skip_on_cran()
    cli <- D1Client("SANDBOX")
    expect_false(is.null(cli))
    expect_match(class(cli), "D1Client")
    expect_match(cli@cn@baseURL, "https://cn-sandbox.test.dataone.org/cn")
})
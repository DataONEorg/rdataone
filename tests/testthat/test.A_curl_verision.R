test_that("curl supports TLS1.3", {
    library(httr)
    ssl_version <- curl_version()$ssl_version
    expect_match(ssl_version, "OpenSSL|LibreSSL|Schannel")
})

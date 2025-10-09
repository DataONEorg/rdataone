test_that("curl supports TLS1.3", {
    ssl_version <- curl::curl_version()$ssl_version
    expect_match(ssl_version, "OpenSSL|LibreSSL|Schannel")
})

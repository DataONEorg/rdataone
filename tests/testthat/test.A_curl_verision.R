test_that("curl supports TLS1.3", {
    library(dataone)
    expect_match(curl::curl_version()$ssl_version, 
                 "OpenSSL|LibreSSL|Schannel")
})

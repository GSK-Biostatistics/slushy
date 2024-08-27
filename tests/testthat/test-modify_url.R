# Tests for modify_url function

test_that("modify_url returns empty string when pkg_url is NULL", {
  skip_if_not(interactive())
  
  pkg_url <- NULL
  config_url <- "https://packagemanager.posit.co/cran"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    ""
  )
})

test_that("modify_url returns pkg_url when config_url is NULL", {
  skip_if_not(interactive())
  
  pkg_url <- "https://packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  config_url <- NULL
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    pkg_url
  )
})

test_that("modify_url returns empty string when both pkg_url and config_url are NULL", {
  skip_if_not(interactive())
  
  config_url <- NULL
  pkg_url <- NULL
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    ""
  )
})

test_that("modify_url returns custom URL when pkg_url has no date component", {
  skip_if_not(interactive())
  
  pkg_url <- "https://example.com/repo/subdirectory"
  config_url <- "https://example.com/repo"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "repo/subdirectory"
  )
})

test_that("modify_url handles subdomains in URLs", {
  skip_if_not(interactive())
  
  config_url <- "https://subdomain.packagemanager.posit.co/cran"
  pkg_url <- "https://subdomain.packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "cran/2024-03-07"
  )
})

test_that("modify_url returns expected custom url (cran with https)", {
  skip_if_not(interactive())
  
  pkg_url <- "https://packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  config_url <- "https://packagemanager.posit.co/cran"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "cran/2024-03-07"
  )
})

test_that("modify_url returns expected custom url (cran without https)", {
  skip_if_not(interactive())
  
  pkg_url <- "packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  config_url <- "packagemanager.posit.co/cran"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "cran/2024-03-07"
  )
})

test_that("modify_url returns expected custom url (custom repo with https)", {
  skip_if_not(interactive())
  
  pkg_url <- "https://my.custom.url.com/custom_cran/subdirectory1/subdirectory2/2024-07-23"
  config_url <- "https://my.custom.url.com/custom_cran"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "custom_cran/subdirectory1/subdirectory2/2024-07-23"
  )
})

test_that("modify_url returns expected custom url (custom repo without https)", {
  skip_if_not(interactive())
  
  pkg_url <- "my.custom.url.com/custom_cran/subdirectory1/subdirectory2/2024-07-23"
  config_url <- "my.custom.url.com/custom_cran"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "custom_cran/subdirectory1/subdirectory2/2024-07-23"
  )
})


# Tests for modify_url function

test_that("modify_url returns expected custom url (cran with https)", {
  skip_if_not(interactive())
  
  config_url <- "https://packagemanager.posit.co/cran"
  pkg_url <- "https://packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "cran/2024-03-07")
})


test_that("modify_url returns expected custom url (cran without https)", {
  skip_if_not(interactive())
  
  config_url <- "packagemanager.posit.co/cran"
  pkg_url <- "packagemanager.posit.co/cran/2024-03-07/bin/windows/contrib/4.3"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "cran/2024-03-07")
})


test_that("modify_url returns expected custom url (custom repo with https)", {
  skip_if_not(interactive())
  
  config_url <- "https://my.custom.url.com/custom_cran"
  pkg_url <- "https://my.custom.url.com/custom_cran/subdirectory1/subdirectory2/2024-07-23"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "custom_cran/subdirectory1/subdirectory2/2024-07-23")
})


test_that("modify_url returns expected custom url (custom repo without https)", {
  skip_if_not(interactive())
  
  config_url <- "my.custom.url.com/custom_cran"
  pkg_url <- "my.custom.url.com/custom_cran/subdirectory1/subdirectory2/2024-07-23"
  
  expect_equal(
    output_url <- modify_url(pkg_url, config_url),
    "custom_cran/subdirectory1/subdirectory2/2024-07-23")
})


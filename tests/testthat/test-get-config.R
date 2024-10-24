test_that("get_config attaches the correct file name", {
  # create a temp config file
  temp_config_file <- tempfile(fileext = ".yml")
  
  # write a simple config contents
  writeLines("
  default:
    config_name: 'my_custom_config'
    my_custom_config:
      key1: 'value1'
      key2: 'value2'
  ", temp_config_file)
  
  # call the get_config
  result <- get_config(temp_config_file)
  
  # check if the correct config list is returned
  expect_equal(result$key1, 'value1')
  expect_equal(result$key2, 'value2')
  
  # check file name attribute is correctly attached
  expect_equal(attr(result, "config_file_name"), temp_config_file)
  
  # clean up temp file
  unlink(temp_config_file)
})


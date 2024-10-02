test_that("update_ignores modifies .gitignore correctly", {
  # create a temp dir
  temp_dir <- tempdir()
  
  # create a temp .gitignore
  gitignore_path <- file.path(temp_dir, ".gitignore")
  writeLines(c("existing_file.txt"), gitignore_path)  # initial content
  
  # create a temp config file
  temp_config_file <- tempfile(fileext = ".yml")
  writeLines("
  default:
    config_name: 'my_custom_config'
    my_custom_config:
      key1: 'value1'
      key2: 'value2'
  ", temp_config_file)
  
  # call update_ignores with the temp dir and the config file name
  slushy:::update_ignores(project = temp_dir, additional_unignore_files = basename(temp_config_file))
  
  # read the contents of the .gitignore file
  updated_gitignore <- readLines(gitignore_path, warn = FALSE)
  
  # expected renv_files including the temp config file unignored
  renv_files <- c("!DESCRIPTION",
                  "!.Rprofile",
                  "!.renvignore",
                  "!renv",
                  "!renv.lock",
                  "!*.Rproj",
                  "existing_file.txt",
                  paste0("!", basename(temp_config_file)))
  
  # check if the updated .gitignore matches the expected renv_files
  expect_equal(sort(updated_gitignore), sort(renv_files))
  
  # clean up temp config file
  unlink(temp_config_file)
})

# test when gitignore has contents
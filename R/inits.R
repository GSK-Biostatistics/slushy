

#' Add environment variable settings to text for .Rprofile
#' @param rprofile_text Current text for .Rprofile
#' @param env_settings List of environment variables to be set
#' @importFrom purrr reduce imap
#' @importFrom rlang is_empty
#' @noRd
add_slushy_rprofile_env_code <- function(rprofile_text, rspm_url, env_settings){

  # use rspm_url's "latest" repo if override not specified
  if (!"RENV_CONFIG_REPOS_OVERRIDE" %in% names(env_settings)){
    env_settings <- c(env_settings, 
                      list(RENV_CONFIG_REPOS_OVERRIDE = paste0("\"", rspm_url, "/latest\"")))
  }
  env_settings_to_text <- imap(env_settings, function(x,y) {
    paste0("Sys.setenv(", y, " = ", x, ")")
  })

  if (!is_empty(env_settings_to_text)){
    rprofile_text <- c(
      rprofile_text,
      "## Set slushy global options and environment variables",
      reduce(env_settings_to_text, c)
    )
  }

  return(rprofile_text)
}

#' Add check to restore the lockfile version of slushy to text for .Rprofile
#' @param rprofile_text Current text for .Rprofile
#' @noRd
add_slushy_rprofile_install_code <- function(rprofile_text){

  c(rprofile_text,
    "## Code to install correct version of slushy if needed",
    "renv::restore(packages = \"slushy\", repos = getOption(\"repos\"), prompt = FALSE)"
  )
}


#' Make activate.R quiet to streamline startup messages for slushy user in text
#' for .Rprofile
#' @param rprofile_text Current text for .Rprofile
#' @noRd
add_slushy_rprofile_activate_code <- function(rprofile_text){

  rprofile_text <- gsub("source(\"renv/activate.R\")", "", rprofile_text, fixed = TRUE)

  rprofile_text <- c(
    rprofile_text,
    "## Code to activate renv",
    "local({",
    "  options(renv.config.synchronized.check = FALSE)",
    "    activated_ok <- tryCatch({",
    "       ok <- utils::capture.output({source(\"renv/activate.R\")}, type = \"output\")",
    "       TRUE",
    "    }, warning = function(w){",
    "       FALSE",
    "  })",
    "  if (!activated_ok){",
    "    lib <- renv::paths$library()",
    "    stop(paste0(\"Problem activating {renv}. May be due to incorrect version\",",
    "                 \" of {renv} already installed to project library.\",",
    "                 \"\\nPlease try the following command followed by a session restart:\",",
    "                 \"\\n`renv::remove(\\\"renv\\\", library = \\\"\", lib, \"\\\")`\"), call. = FALSE)",
    "  }",
    "})"
  )

  return(rprofile_text)
}

#' Auto check slushy status in text for .Rprofile
#' @param rprofile_text Current text for .Rprofile
#' @importFrom rprojroot is_rstudio_project
#' @noRd
add_slushy_rprofile_status_code <- function(rprofile_text, pkg_deps_ok){

  rprofile_text <- c(
    rprofile_text,
    c(
      "## Code to check slushy status",
      "local({",
      paste0("  slushy::slushy_status(pkg_deps_ok = ", pkg_deps_ok, ")"),
      "})"
    )
  )

  return(rprofile_text)
}

#' Add custom startup code to text for .Rprofile
#' @param rprofile_text Current text for .Rprofile
#' @param custom_startup Named list of code to be run prior to and following
#'   core slushy startup. Code should be provided as character strings and named
#'   "pre" and "post", depending on the order.
#' @importFrom rprojroot is_rstudio_project
#' @noRd
add_slushy_rprofile_custom_code <- function(rprofile_text, custom_startup){

  if (!is.null(custom_startup$pre)){
    start_lines <- min(grep("^### SLUSHY RPROFILE - START ###", rprofile_text))

    pre_slushy_lines <- seq_len(start_lines)
    post_slushy_lines <- (start_lines+1):length(rprofile_text)

    rprofile_text <- c(rprofile_text[pre_slushy_lines],
                       "## Custom startup code for pre-slushy",
                       custom_startup$pre,
                       rprofile_text[post_slushy_lines])
  }
  if (!is.null(custom_startup$post)){
    rprofile_text <- c(rprofile_text,
                       "## Custom startup code for post-slushy",
                       custom_startup$post)
  }

  return(rprofile_text)
}



#' Add slushy startup text to .Rprofile
#' @param project path to project
#' @param config List of slushy configs
#' @noRd
add_slushy_rprofile_code <- function(project = proj_root(),
                                     config){

  rprofile_file <- file.path(project, ".Rprofile")

  if (file.exists(rprofile_file)){
    rprofile_text <- readLines(rprofile_file, warn = FALSE)
  } else {
    rprofile_text <- ""
  }

  rprofile_text <- c(rprofile_text, "### SLUSHY RPROFILE - START ###") %>%
    add_slushy_rprofile_env_code(config$rspm_url, config$environment) %>%
    add_slushy_rprofile_activate_code() %>%
    add_slushy_rprofile_install_code() %>%
    add_slushy_rprofile_status_code(config$pkg_deps_ok) %>%
    add_slushy_rprofile_custom_code(config$startup)

  rprofile_text <- c(rprofile_text, "### SLUSHY RPROFILE - END ###")

  writeLines(text = rprofile_text,
             sep = "\n",
             con = rprofile_file)

}


#' Take extra measures to ensure slushy works as expected in a git project
#' 
#' Makes modifications to the .gitignore and .renvignore files to ensure the following:
#' - critical files related to slushy are not accidentally ignored by git
#' - R packages being used in the project are not accidentally overlooked
#' 
#' @param project location of project
#' 
#' @noRd
update_ignores <- function(project = proj_root()){
  
  # add slushy files to gitignore
  git_ignore_file <- file.path(project, ".gitignore")
  renv_files <- c("!DESCRIPTION",
                  "!.Rprofile",
                  "!.renvignore",
                  "!renv",
                  "!renv.lock",
                  "!*.Rproj")
  
  if (file.exists(git_ignore_file)){
    git_ignore <- readLines(git_ignore_file, warn = FALSE)
    
    git_ignore_add <- setdiff(renv_files, git_ignore)
    
    if (length(git_ignore_add)>0){
      git_ignore <- c(git_ignore, git_ignore_add)
       
      writeLines(text = git_ignore,
                 sep = "\n",
                 con = file.path(project, ".gitignore")) 
    }
  }
  
  # create renvignore file to be used for dependency checking in lieu of gitignore
  if (! file.exists(".renvignore")){
    writeLines(text = "",
               sep = "\n",
               con = file.path(project, ".renvignore"))
  }
}

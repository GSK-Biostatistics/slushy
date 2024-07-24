#' Get project path
#'
#' @param ... Path relative to root directory
#' @param project Project path
#'
#' @importFrom rprojroot is_rstudio_project
#' @return Full project path
#' @noRd
proj_root <- function(..., project = NULL){

  if (is.null(project)){

    project <- tryCatch({
      is_rstudio_project$find_file()
    },
    error = function(e){
      getwd()
    }
    )

  }

  file.path(
    project,
    ...
  )
}


#' Check that environment is clean
#' @return Error message if environment needs to be restarted
#' @noRd
confirm_clean_env <- function(){

  search_Res <- search()

  not_clean <- any(
    !search_Res %in% c(
      ".GlobalEnv",
      "renv:shims",
      "devtools_shims",
      "org:r-lib",
      "tools:rstudio",
      "package:stats",
      "package:graphics",
      "package:grDevices",
      "package:datasets",
      "package:utils",
      "package:methods",
      "Autoloads",
      "package:base",
      "package:slushy",
      "tools:callr"
    )
  )

  if(not_clean){
    stop("Environment needs to be restarted before this function can be run.")
  }

}



#' Get closest Package Manager snapshot date URL
#' @param date CRAN snapshot date (as date with format yyyy-mm-dd)
#' @param repo_url Package manager base URL
#' @return URL
#' @noRd
closest_cran_repo <- function(date = NULL, repo_url){

  # fix date, if not specified or in the future
  if (is.null(date) || date > Sys.Date()){
    date <- Sys.Date()
  }

  res <- NULL
  date <- date+1

  while(is.null(res)){
    date <- date-1
    url <- file.path(repo_url, date)
    url_full <- file.path(url, "src/contrib/PACKAGES")

    res <- tryCatch({
      success <- suppressWarnings(readLines(url_full))
      url
    },
    error = function(cond){
      NULL
    }
    )
  }

  return(res)
}

#' Get modified repos to be set
#' @param date CRAN snapshot date (as string with format yyyy-mm-dd)
#' @param repo_url Package manager base URL
#'
#' @return Named vector of repo URLs
#' @noRd
get_repos <- function(date = NULL,
                      repo_url){

  if(!is.null(date)) date <- as.Date(date)

  cran_repo <- closest_cran_repo(date, repo_url)
  repos <- getOption("repos")

  repos <- c("RSPM" = cran_repo,  # To match conventions of lock file, in which all packages from Posit Package Manager are labeled "RSPM"
             repos[setdiff(names(repos), c("RSPM", "CRAN"))]) # drop existing RSPM, CRAN repos but keep the rest

  return(repos)
}

#' Create a new DESCRIPTION file
#'
#' @param file file path
#'
#' @importFrom desc description
#' @noRd
create_desc_file <- function(file){
  desc_file <- description$new("!new")
  desc_file$write(file = file)
}

#' document pkgs in DESCRIPTION
#'
#' @param pkgs character vector of package names
#' @param project Path to project
#'
#' @importFrom desc desc_del_deps
#' @noRd
document_pkgs <- function(pkgs, project = proj_root()){

  file <- file.path(project, "DESCRIPTION")

  if(file.exists(file)){
    desc_del_deps(file = file)
  }
  add_desc_deps(pkgs, file = file)
}


#' Add packages to DESCRIPTION as Imports
#'
#' @param pkgs Character vector of package names
#' @param file Path to DESCRIPTION file
#' @param replace Replace original set of Imports? Defaults to FALSE
#'
#' @importFrom desc desc_get_deps desc_set_deps
#' @noRd
add_desc_deps <- function(pkgs, file = proj_root("DESCRIPTION"), replace = FALSE){

  if(!file.exists(file)){
    create_desc_file(file = file)
  }
  existing_deps <- desc_get_deps(file = file)

  if (length(pkgs)>0){
    deps <- data.frame(type = "Imports", package = gsub("@.*","", pkgs), version = "*", stringsAsFactors = FALSE)
    if (!replace){
      deps <- rbind(existing_deps, deps)
    }
  } else {
    deps <- existing_deps
  }

  val <- desc_set_deps(
    deps = unique(deps),
    file = file
  )
}


#' Retrieve all agreed upon packages, including base R/recommended
#'
#' @param config Config list
#' @return Character vector of package names
#' @noRd
get_agreed_pkgs <- function(config = get_config()){

  pkgs_base <- installed.packages(lib.loc = file.path(R.home(), "library"),
                                  priority = c("base", "recommended")) %>% row.names()

  pkgs_config <- config$pkgs
  c(pkgs_base, pkgs_config) %>% unique()
}

# install packages and dependencies
#' @param pkg Name of package as string
#' @param library The R library to be used. If NULL, the active project library will be used.
#' @param repos Repos to install packages from
#' @param check_agreed Check that package has been defined as an agreed upon package (defined in config), and inform the user if not
#' @param config Slushy config list containing packages
#'
#' @noRd
#'
#' @importFrom renv install
#' @importFrom utils capture.output
#' @importFrom cli cli_alert_warning cli_progress_step cli_progress_done
try_install <- function(pkg,
                        library = NULL,
                        repos = NULL,
                        check_agreed = FALSE,
                        config = get_config()){
  
  if (check_agreed){
    if (!pkg %in% get_agreed_pkgs(config)){
      cli_alert_warning(paste0("Note: `", pkg, "` is not part of the agreed upon set of packages."))
    }
  }
  
  res <- ""
  id  <- cli_step_notime(paste0("Installing `", pkg, "` ... {res}"))
  
  success <- tryCatch({
    p <- capture.output({
      install_result <- install(pkg, library = library, repos = repos, prompt = FALSE)
    })
    
    pkg_version <- install_result[[pkg]]$Version
    pkg_url <- attr(install_result[[pkg]], "url")
    pkg_url_trimmed <- str_extract(pkg_url, "(cran|rspm|warp.*?)/\\d{4}-\\d{2}-\\d{2}")
    
    res <- paste0("success (version ", pkg_version, ", ", pkg_url_trimmed, ")")
    cli_progress_done(id = id, result = "done")
    pkg
  }, error = function(e){
    res <- "not available in this snapshot"
    cli_progress_done(id = id, result = "failed")
    quiet_remove(pkg)
    return(NULL)
  })
  
  invisible(success)
}



# copy slushy package and install dependencies if needed
#' @param slushy_loc Path to installed version of slushy
#' @param library The R library to be used. If NULL, the active project library will be used.
#'
#' @noRd
#'
#' @importFrom renv install remove
#' @importFrom utils capture.output
#' @importFrom cli cli_alert_warning cli_progress_step cli_progress_done
try_install_slushy <- function(slushy_loc,
                               library = NULL){

  res <- ""
  id  <- cli_step_notime(paste0("Installing `slushy`...{res}"))

  if (is.null(library)){
    library <- .libPaths()[1]
  }
  success <- tryCatch({
    if (!slushy_loc == file.path(library,"slushy")){
      if (dir.exists(file.path(library, "slushy"))){
        slushy_rmv <- capture.output(remove("slushy", library = library))
      }
      p <- capture.output({file.copy(from = slushy_loc, to = library, recursive = TRUE)})
    }
    res <- "success"
    cli_progress_done(id = id, result = "done")
  }, error = function(e){
    res <- "cannot be copied from local source"
    cli_progress_done(id = id, result = "failed")
    return(NULL)
  })

  invisible(success)

}



#' remove existing package from library if unavailable in desired CRAN snapshot
#' @param pkg Name of package as string
#'
#' @importFrom renv remove
#' @importFrom utils capture.output
#' @importFrom cli cli_alert_danger
#' @noRd
quiet_remove <- function(pkg){
  tryCatch({
    p <- capture.output({remove(pkg)})
  }, error = function(e){
    cli_alert_danger("Unable to remove `",pkg,"`")
    e
  })
}



#' take renv::snapshot of environment
#'
#' @param repos Repos to snapshot. Defaults to those set in current session
#' @param project path to project
#' @param quiet Suppress confirmation message? defaults to FALSE
#' @param force Force snapshot creation? defaults to FALSE
#'
#' @importFrom renv snapshot
#' @importFrom utils capture.output
#' @importFrom cli cli_alert_info
#'
#' @noRd
update_snapshot <- function(repos = getOption("repos"),
                            project = project,
                            quiet = FALSE,
                            force = FALSE){
  val <- capture.output({
    snapshot(project = project, type = "explicit", repos = repos, prompt = FALSE, force = force)
  })
  if(!quiet){
    val <- val[length(val)]
    if (substr(val, 1, 2)=="* "){
      val <- substr(val, 3, nchar(val))
    }
    cli_alert_info(val)
  }
}

#' remove slushy text from rprofile
#'
#' @param project Path to project
#' @param remove_empty If the .Rprofile is empty after slushy text has been
#'   removed, should it be deleted? Defaults to TRUE
#' @noRd
clean_rprofile <- function(project = proj_root(),
                           remove_empty = TRUE){

  rprofile_file <- file.path(project, ".Rprofile")

  if (file.exists(rprofile_file)){
    rprofile_text <- readLines(rprofile_file, warn = FALSE)

    if (any(grepl("### SLUSHY RPROFILE", rprofile_text))){

      lines_slushy <- c(min(grep("### SLUSHY RPROFILE - START ###", rprofile_text)),
                        max(grep("### SLUSHY RPROFILE - END ###", rprofile_text)))

      if (length(lines_slushy)==2){
        lines_to_remove <- lines_slushy[1]:lines_slushy[2]

        rprofile_text <- rprofile_text[-lines_to_remove]

        rprofile_empty <- length(rprofile_text)==0 || trimws(paste0(rprofile_text, collapse = "")) == ""

        if (!rprofile_empty){
          writeLines(text = rprofile_text,
                     sep = "\n",
                     con = rprofile_file)
        } else {

          if (remove_empty){
            file.remove(rprofile_file)
          }

        }

      }
    }
  }
}


#' Adaptation of cli::cli_progress_step() - modified to remove timestamp from
#' print messages
#' @param msg Message to include in the step
#' @importFrom cli cli_progress_bar cli_progress_update
#' @noRd
cli_step_notime <- function(msg) {

  format <- paste0("{.alert-info ", msg, "}")
  format_done <- paste0("{.alert-success ", msg, "}")
  format_failed <- paste0("{.alert-danger ", msg, "}")

  opt <- options(cli.progress_show_after = 0)
  on.exit(options(opt), add = TRUE)
  id <- cli_progress_bar(
    type = "custom",
    format = format,
    format_done = format_done,
    format_failed = format_failed,
    clear = FALSE,
    current = TRUE,
    .auto_close = TRUE,
    .envir = parent.frame()
  )

  cli_progress_update(id = id, force = TRUE, .envir = parent.frame())

  invisible(id)
}


